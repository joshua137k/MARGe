var currentCytoscapeInstance = null;
var textTraceHistory = [];
var autoDelayTimer = null;



window.stopAutoDelay = stopAutoDelay;




function extractMermaidPositions(mermaidContainerId) {
    const positions = {};
    const mermaidContainer = document.getElementById(mermaidContainerId);
    if (!mermaidContainer) return positions;
    const nodeElements = mermaidContainer.querySelectorAll('g.node');
    nodeElements.forEach(node => {
        const fullId = node.id;
        const idParts = fullId.split('-');
        if (idParts.length > 1 && idParts[0] === 'flowchart') {
            const nodeName = idParts[1];
            const transform = node.getAttribute('transform');
            const translateRegex = /translate\(([^,]+),([^)]+)\)/;
            const match = transform.match(translateRegex);
            if (match) positions[nodeName] = { x: parseFloat(match[1]), y: parseFloat(match[2]) };
        }
    });
    return positions;
}


function extractMermaidEdgeLabelPositions(mermaidContainerId) {
    const positions = {};
    const mermaidContainer = document.getElementById(mermaidContainerId);
    if (!mermaidContainer) return positions;
    const labelElements = mermaidContainer.querySelectorAll('g.edgeLabel');
    labelElements.forEach(label => {
        const span = label.querySelector('span.edgeLabel');
        const labelText = span ? span.textContent.trim() : '';
        if (labelText) {
            const transform = label.getAttribute('transform');
            const translateRegex = /translate\(([^,]+),([^)]+)\)/;
            const match = transform.match(translateRegex);
            if (match) positions[labelText] = { x: parseFloat(match[1]), y: parseFloat(match[2]) };
        }
    });
    return positions;
}


function changeEdgeStyle(styleName) {
    if (!currentCytoscapeInstance) return;

    const edgesToStyle = currentCytoscapeInstance.edges('.simple-conn, .from-action-node');

    edgesToStyle.removeClass('arc taxi straight');

    if (styleName) {
        edgesToStyle.addClass(styleName);
    }
}

function renderCytoscapeGraph(mainContainerId, combinedJsonData, isFirstRender) {
    var mainContainer = document.getElementById(mainContainerId);
    if (!mainContainer) {
        console.error('Main container not found: ' + mainContainerId);
        return;
    }

    if (!isFirstRender) {
        try {
            var data = JSON.parse(combinedJsonData);
            if (currentCytoscapeInstance) {
                 if (data.lastTransition) {
                    textTraceHistory.push(data.lastTransition.to);
                } else { 
                    if (textTraceHistory.length > 1) { 
                        textTraceHistory.pop();
                    }
                }
                updateSidePanel(mainContainerId + '_panel', data.panelData);
                
                currentCytoscapeInstance.json({ elements: data.graphElements });
                
                if (data.lastTransition) {
                    var trans = data.lastTransition;
                    var from = trans.from;
                    var to = trans.to;
                    var lbl = trans.lbl;
                    var actionNodeId = `event_${from}_${to}_${lbl}`;
                    var edgeToActionModeId = `s_to_a_${from}_${actionNodeId}`;
                    var edgeFromActionNodeId = `a_to_s_${actionNodeId}_${to}`;
                    var selector = `#${actionNodeId}, #${edgeToActionModeId}, #${edgeFromActionNodeId}`;
                    
                    var elementsToFlash = currentCytoscapeInstance.elements(selector);

                    if (elementsToFlash && elementsToFlash.length > 0) {
                        elementsToFlash.addClass('transition-flash');
                        setTimeout(function() {
                            elementsToFlash.removeClass('transition-flash');
                        }, 1000); 
                    }
                }
            }
        } catch (e) {
            console.error("Falha ao atualizar o grafo:", e);
        }
        return; 
    }

    let attempts = 0;
    const maxAttempts = 50; 
    
    const poller = setInterval(function() {

        const mermaidSvg = document.querySelector('.mermaid > svg');

        if (mermaidSvg || attempts >= maxAttempts) {
            clearInterval(poller); 

            if (!mermaidSvg) {
                console.warn("O grafo do Mermaid não foi encontrado. Renderizando Cytoscape com layout padrão.");
            }
            setupInitialCytoscape(mainContainerId, combinedJsonData);
        } else {
            attempts++;
        }
    }, 100); 
}


async function setupInitialCytoscape(mainContainerId, combinedJsonData) {
    var mainContainer = document.getElementById(mainContainerId);

    try {
        var data = JSON.parse(combinedJsonData);

        const graphId = generateGraphId(data.graphElements);
        if (!hasExistingLayoutsInLocalStorage()) {
            console.log("O LocalStorage está vazio. Tentando carregar layouts do arquivo padrão...");
            await loadDefaultLayoutsFromSeedFile();
        }
        
        textTraceHistory = [];
        const target = data.graphElements.find(
            el => el.classes && el.classes.includes("current-state")
        );
        if (target) {
            textTraceHistory.push(target.data.id);
        }

        const mermaidNodePositions = extractMermaidPositions('id-1996100447Box');
        const mermaidLabelPositions = extractMermaidEdgeLabelPositions('id-1996100447Box');
        
        let layoutName = 'presset'; 

        if (Object.keys(mermaidNodePositions).length > 0 || Object.keys(mermaidLabelPositions).length > 0) {
            console.log("Posições do Mermaid encontradas! Aplicando layout 'preset'.");
            layoutName = 'preset'; 
            
            const idMap = new Map();
            const labelMap = new Map();

            data.graphElements.forEach(el => {
                if (!el.data) return;
                if (el.data.id) {
                    idMap.set(el.data.id, el);
                }
                if (el.data.label && el.classes && el.classes.includes('event-node')) {
                    labelMap.set(el.data.label, el);
                }
            });

            data.graphElements.forEach(el => {
                if (!el.data) return;

                if (el.classes && el.classes.includes('state-node')) {
                    const pos = mermaidNodePositions[el.data.id];
                    if (pos) {
                        el.position = pos;
                    }
                } else if (el.classes && el.classes.includes('event-node')) {
                    const pos = mermaidLabelPositions[el.data.label];
                     if (pos) {
                        el.position = pos;
                    }
                }
            });
            data.graphElements.forEach(el => {
                if (el.classes && el.classes.includes('event-node') && !el.position) {
                    const idParts = el.data.id.split('_');
                    if (idParts.length >= 3) {
                        const sourceName = idParts[1]; 
                        const targetName = idParts[2]; 

                  
                        let sourceNode = idMap.get(sourceName);
                        if (!sourceNode) {
                            sourceNode = labelMap.get(sourceName);
                        }

                        let targetNode = idMap.get(targetName);
                        if (!targetNode) {
                            targetNode = labelMap.get(targetName);
                        }

                        if (sourceNode && sourceNode.position && targetNode && targetNode.position) {
                            el.position = {
                                x: (sourceNode.position.x + targetNode.position.x) / 2,
                                y: (sourceNode.position.y + targetNode.position.y) / 2
                            };
                        } else {
                             console.warn(`Não foi possível calcular a posição para '${el.data.id}'. Posição de origem/destino não encontrada para '${sourceName}' ou '${targetName}'.`);
                        }
                    }
                }
            });
        }
        
        if (currentCytoscapeInstance) {
            currentCytoscapeInstance.destroy();
        }
        mainContainer.innerHTML = '';
        mainContainer.style.display = 'flex';
        mainContainer.style.height = '600px';

        var panelDiv = document.createElement('div');
        panelDiv.id = mainContainerId + '_panel';
        panelDiv.style.width = '200px';
        panelDiv.style.padding = '10px';
        panelDiv.style.borderRight = '1px solid #414868';
        panelDiv.style.overflowY = 'auto';

        var graphDiv = document.createElement('div');
        graphDiv.id = mainContainerId + '_graph';
        graphDiv.style.flexGrow = '1';
        graphDiv.style.backgroundColor = '#1a1b26';
        
        mainContainer.appendChild(panelDiv);
        mainContainer.appendChild(graphDiv);

        updateSidePanel(panelDiv.id, data.panelData);

        var cy = cytoscape({
            container: graphDiv,
            elements: data.graphElements,
            style: [ 
                { selector: 'node', style: { 'label': 'data(label)', 'text-valign': 'center', 'color': '#c0caf5', 'font-family': 'sans-serif', 'font-weight': 'bold', 'text-outline-width': 2, 'text-outline-color': '#1a1b26' } },
                { selector: 'edge', style: { 'width': 2, 'curve-style': 'bezier', 'line-color': '#565f89', 'target-arrow-color': '#565f89', 'label': 'data(label)', 'color': '#c0caf5', 'text-outline-color': '#1a1b26', 'text-outline-width': 2, 'font-size': '14px' } },
                { selector: 'node.state-node', style: { 'background-color': '#7aa2f7', 'shape': 'ellipse', 'width': 50, 'height': 50, 'border-width': 3, 'border-color': '#414868','text-wrap': 'wrap','text-valign': 'center' } },
                {selector: 'node.has-invariant',style: {'label': (ele) => ele.data('label') + '\n[' + ele.data('invariant') + ']'}},
                { selector: '.current-state', style: { 'background-color': '#9ece6a', 'border-color': '#c0caf5' } },
                { selector: 'node.event-node', style: { 'background-color': '#414868', 'shape': 'rectangle', 'width': 50, 'height': 30, 'border-width': 2, 'border-color': '#565f89' } },
                { selector: 'edge', style: { 'target-arrow-shape': 'none' } },
                
                { selector: 'edge.from-action-node', style: { 'target-arrow-shape': 'triangle' } },
                { selector: '.enable-rule', style: { 'line-color': '#7aa2f7', 'target-arrow-color': '#7aa2f7' } },
                { selector: '.disable-rule', style: { 'line-color': '#f7768e', 'target-arrow-color': '#f7768e' } },
                { selector: 'edge.enable-rule.to-target', style: { 'target-arrow-shape': 'triangle-tee' } },
                { selector: 'edge.disable-rule.to-target', style: { 'target-label': 'X', 'target-text-offset': 5, 'color': '#f7768e','font-size': '12px' } },
                { selector: '.disabled', style: { 'line-style': 'dashed', 'background-opacity': 0.6, 'border-style': 'dashed', 'opacity': 0.7 } },
                { selector: 'node.transition-flash', style: {'background-color': '#ff9e64','border-color': 'white' }},
                { selector: 'edge.transition-flash', style: {'line-color': '#ff9e64','target-arrow-color': '#ff9e64','source-arrow-color': '#ff9e64','width': 4}},
                { selector: '.trace-path', style: { 'line-color': '#7dcfff', 'target-arrow-color': '#7dcfff', 'source-arrow-color': '#7dcfff', 'width': 3.5, 'opacity': 0.9 } },
                { selector: '.compound-parent', style: { 'background-color': '#1a1b26', 'background-opacity': 1, 'border-color': '#c0caf5', 'border-width': 2,'content': 'data(label)', 'text-valign': 'top','text-halign': 'center','color': '#c0caf5','font-weight': 'bold','font-size': '16px'} },
                {selector: 'edge.simple-conn.arc',style: { 'curve-style': 'unbundled-bezier', 'control-point-distances': '80', 'control-point-weights': '0.9', 'target-arrow-shape': 'none' }},
                {selector: 'edge.from-action-node.arc',style: { 'curve-style': 'unbundled-bezier', 'control-point-distances': '80', 'control-point-weights': '0.1', 'target-arrow-shape': 'triangle' }},
                {selector: 'edge.simple-conn.taxi',style: { 'curve-style': 'taxi', 'taxi-direction': 'vertical', 'taxi-turn': '25px', 'target-arrow-shape': 'none' }},
                {selector: 'edge.from-action-node.taxi',style: { 'curve-style': 'taxi', 'taxi-direction': 'vertical', 'taxi-turn': '25px', 'target-arrow-shape': 'triangle' }},
                {selector: 'edge.simple-conn.straight',style: { 'curve-style': 'straight', 'target-arrow-shape': 'none' }},
                {selector: 'edge.from-action-node.straight',style: { 'curve-style': 'straight', 'target-arrow-shape': 'triangle' }},

            ],
            layout: {
                name: layoutName,
                rankDir: 'LR', fit: true, padding: 50, spacingFactor: 1.2, nodeSep: 60, rankSep: 70, edgeSep: 10
            }
        });
        
        const loadedFromLocal = loadLayoutFromLocalStorage(cy,graphId);
        console.log(loadedFromLocal);

        if (!loadedFromLocal) {
            console.log("Nenhum layout local encontrado. Usando layout padrão.");
           
        }

        cy.on('dragfree', 'node', function() {
            autoSaveLayoutToLocalStorage(cy,graphId);
        });

        cy.on('tap', 'node.event-node.enabled', function(evt){
            var node = evt.target;
            var nodeId = node.id();
            var parts = nodeId.split('_');
            if (parts.length >= 4) {
                var from = parts[1];
                var to = parts[2];
                var lbl = parts.slice(3).join('_'); 
                var edgeJson = JSON.stringify({ "from": from, "to": to, "lbl": lbl });
                CaosConfig2.takeStep(edgeJson);
            }
        });
        
        cy.on('mouseover', 'node.event-node.enabled', function(e) { e.cy.container().style.cursor = 'pointer'; });
        cy.on('mouseout', 'node.event-node.enabled', function(e) { e.cy.container().style.cursor = 'default'; });

        cy.on('tap', 'edge.has-details', function(evt){
            var edge = evt.target;
            
            if (edge.data('is_expanded')) {
                edge.data('label', edge.data('short_label'));
                edge.data('is_expanded', false);
            } else {
                edge.data('label', edge.data('full_label'));
                edge.data('is_expanded', true);
            }
        });

        cy.on('mouseover', 'edge.has-details', function(e) {
            e.cy.container().style.cursor = 'pointer';
        });
        cy.on('mouseout', 'edge.has-details', function(e) {
            e.cy.container().style.cursor = 'default';
        });


        currentCytoscapeInstance = cy;
        updateButtonHandlers(cy, graphId);
        changeEdgeStyle('straight'); 

    } catch (e) {
        console.error("Falha ao analisar ou renderizar a UI:", e);
        if (mainContainer) {
            mainContainer.innerText = "Erro ao construir a UI. Verifique o console.";
        }
    }
}


function updateSidePanel(panelId, panelData) {
    var panelDiv = document.getElementById(panelId);
    if (!panelDiv) return;
    let savedDelayValue = '1.0';
    const existingInput = document.getElementById('autoDelayIntervalInput');
    if (existingInput && existingInput.value) {
        savedDelayValue = existingInput.value;
    }

    let savedEdgeStyle = 'straight'; 
    const existingEdgeSelector = document.getElementById('edgeStyleSelector');
    if (existingEdgeSelector) {
        savedEdgeStyle = existingEdgeSelector.value;
    }

    let savedLayout = 'preset'; 
    const existingLayoutSelector = document.getElementById('layoutSelector');
    if (existingLayoutSelector) {
        savedLayout = existingLayoutSelector.value;
    }
    panelDiv.innerHTML = '';

    var undoButton = document.createElement('button');
    undoButton.innerText = 'undo';
    undoButton.onclick = function() { CaosConfig2.undoStep(); }; 
    undoButton.disabled = !panelData.canUndo;
    panelDiv.appendChild(undoButton);
    
    panelDiv.appendChild(document.createElement('hr'));

    if (textTraceHistory && textTraceHistory.length > 0) {
        var traceTitle = document.createElement('p');
        traceTitle.innerText = 'History Trace:';
        traceTitle.style.fontWeight = 'bold';
        traceTitle.style.marginTop = '10px';
        panelDiv.appendChild(traceTitle);

        var traceText = document.createElement('p');
        traceText.innerText = textTraceHistory.join(' -> '); 
        traceText.style.wordBreak = 'break-all';
        traceText.style.fontFamily = 'monospace';
        traceText.style.fontSize = '14px';
        panelDiv.appendChild(traceText);
        
        panelDiv.appendChild(document.createElement('hr'));
    }

    const clocks = panelData.clocks || {};
    const variables = panelData.variables || {};

    if (Object.keys(clocks).length > 0) {
        var clocksTitle = document.createElement('p');
        clocksTitle.innerText = 'Clocks:';
        clocksTitle.style.fontWeight = 'bold';
        panelDiv.appendChild(clocksTitle);
        var clocksList = document.createElement('ul');
        clocksList.style.listStyleType = 'none';
        clocksList.style.paddingLeft = '10px';
        let num = Number(savedDelayValue);      

        const strValue = savedDelayValue.toString();
        const decimalPlaces = (strValue.split('.')[1] || '').length;

        for (const [name, value] of Object.entries(clocks)) {
            var clockItem = document.createElement('li');
            clockItem.innerText = `${name}: ${value.toFixed(decimalPlaces)}`;
            clocksList.appendChild(clockItem);
        }
        panelDiv.appendChild(clocksList);
    }
    
    if (Object.keys(variables).length > 0) {
        var varsTitle = document.createElement('p');
        varsTitle.innerText = 'Variables:';
        varsTitle.style.fontWeight = 'bold';
        panelDiv.appendChild(varsTitle);
        var varsList = document.createElement('ul');
        varsList.style.listStyleType = 'none';
        varsList.style.paddingLeft = '10px';
        for (const [name, value] of Object.entries(variables)) {
            var varItem = document.createElement('li');
            varItem.innerText = `${name}: ${value}`;
            varsList.appendChild(varItem);
        }
        panelDiv.appendChild(varsList);
    }

    if (Object.keys(clocks).length > 0 || Object.keys(variables).length > 0) {
        panelDiv.appendChild(document.createElement('hr'));
    }


    var title = document.createElement('p');
    title.innerText = 'Enabled transitions:';
    title.style.fontWeight = 'bold';
    panelDiv.appendChild(title);

    if (panelData.enabled.length === 0) {
        panelDiv.appendChild(document.createTextNode('- Deadlock -'));
    } else {
        panelData.enabled.forEach(function(edge) {
            var transButton = document.createElement('button');
            transButton.innerText = edge.label;
            transButton.style.display = 'block';
            transButton.style.width = '100%';
            transButton.style.marginBottom = '5px';
            if (edge.isDelay) {
                transButton.onclick = function() { handleDelayClick(); };
            } else {
                transButton.onclick = function() {
                    stopAutoDelay(); 
                    CaosConfig2.takeStep(JSON.stringify(edge));
                };
            }
            panelDiv.appendChild(transButton);
        });
    }

    const hasDelay = panelData.enabled.some(t => t.isDelay);
    if (hasDelay) {
        var timeControlDiv = document.createElement('div');
        timeControlDiv.style.marginTop = '10px';
        timeControlDiv.style.border = '1px solid #414868';
        timeControlDiv.style.padding = '8px';
        timeControlDiv.style.borderRadius = '5px';
        
        var checkboxDiv = document.createElement('div');
        var autoDelayCheckbox = document.createElement('input');
        autoDelayCheckbox.type = 'checkbox';
        autoDelayCheckbox.id = 'autoDelayCheckbox';
        autoDelayCheckbox.onchange = function() { toggleAutoDelay(this.checked); };
        
        var autoDelayLabel = document.createElement('label');
        autoDelayLabel.htmlFor = 'autoDelayCheckbox';
        autoDelayLabel.innerText = ' Auto-advance time';
        
        checkboxDiv.appendChild(autoDelayCheckbox);
        checkboxDiv.appendChild(autoDelayLabel);
        timeControlDiv.appendChild(checkboxDiv);
        
        var delayInputDiv = document.createElement('div');
        delayInputDiv.style.marginTop = '8px';

        var delayLabel = document.createElement('label');
        delayLabel.htmlFor = 'autoDelayIntervalInput';
        delayLabel.innerText = 'Delay (s): ';
        
        var delayInput = document.createElement('input');
        delayInput.type = 'number';
        delayInput.id = 'autoDelayIntervalInput';
        
        delayInput.value = savedDelayValue; 
        delayInput.min = '0.1'; 
        delayInput.step = '0.1'; 
        delayInput.style.width = '60px';

        delayInput.onchange = function() {
            if (autoDelayTimer) { 
                stopAutoDelay();
                toggleAutoDelay(true); 
            }
        };

        delayInputDiv.appendChild(delayLabel);
        delayInputDiv.appendChild(delayInput);
        timeControlDiv.appendChild(delayInputDiv);
        
        panelDiv.appendChild(timeControlDiv);
        
        if (autoDelayTimer) {
             autoDelayCheckbox.checked = true;
        }
    } else {
        stopAutoDelay();
    }
    panelDiv.appendChild(document.createElement('hr'));

    var layoutSelectorContainer = document.createElement('div');
    layoutSelectorContainer.style.marginTop = '20px';
    var layoutLabel = document.createElement('label');
    layoutLabel.innerText = 'Layout: ';
    layoutLabel.htmlFor = 'layoutSelector';
    var layoutSelector = document.createElement('select');
    layoutSelector.id = 'layoutSelector';
    layoutSelector.innerHTML = `
        <option value="preset">Sincronizado</option>
        <option value="dagre">Dagre (Hierárquico)</option>
        <option value="cose">Cose (Forças)</option>
        <option value="circle">Círculo</option>
        <option value="concentric">Concêntrico</option>
        <option value="breadthfirst">Busca em Largura</option>
        <option value="grid">Grade</option>
        <option value="random">Aleatório</option>
    `;
    layoutSelectorContainer.appendChild(layoutLabel);
    layoutSelectorContainer.appendChild(layoutSelector);
    panelDiv.appendChild(layoutSelectorContainer); 

    var edgeStyleContainer = document.createElement('div');
    edgeStyleContainer.style.marginTop = '10px';
    var edgeStyleLabel = document.createElement('label');
    edgeStyleLabel.innerText = 'Estilo de Aresta: ';
    edgeStyleLabel.htmlFor = 'edgeStyleSelector';
    var edgeStyleSelector = document.createElement('select');
    edgeStyleSelector.id = 'edgeStyleSelector';
    edgeStyleSelector.innerHTML = `
        <option value="arc">Curvo (Arco)</option>
        <option value="taxi">Reto (Circuito)</option>
        <option value="straight">Direto</option>
    `;
    
    edgeStyleSelector.value = savedEdgeStyle;

    edgeStyleSelector.onchange = function(event) {
        changeEdgeStyle(event.target.value);
    };

    edgeStyleContainer.appendChild(edgeStyleLabel);
    edgeStyleContainer.appendChild(edgeStyleSelector);
    panelDiv.appendChild(edgeStyleContainer);

    layoutSelector.value = savedLayout;

    layoutSelector.onchange = function(event) {
        var layoutName = event.target.value;
        var layoutOptions;
        switch (layoutName) {
            case 'dagre':
                layoutOptions = { name: 'dagre', rankDir: 'LR', fit: true, padding: 50, spacingFactor: 1.5, nodeSep: 60, rankSep: 70, edgeSep: 10 };
                break;
            case 'cose':
                layoutOptions = { name: 'cose', animate: true, fit: true, padding: 30, nodeRepulsion: 9000, idealEdgeLength: 100, componentSpacing: 100 };
                break;
            case 'circle':
                layoutOptions = { name: 'circle', fit: true, padding: 30 };
                break;
            case 'concentric':
                layoutOptions = { name: 'concentric', fit: true, padding: 30, concentric: function( node ){ return node.degree(); }, levelWidth: function( nodes ){ return 2; } };
                break;
            case 'breadthfirst':
                layoutOptions = { name: 'breadthfirst', fit: true, padding: 30, directed: true };
                break;
            case 'grid':
                layoutOptions = { name: 'grid', fit: true, padding: 30 };
                break;
            case 'random':
                layoutOptions = { name: 'random', fit: true, padding: 30 };
                break;
  
            default:
                 layoutOptions = { name: 'preset', fit: true, padding: 30 };
        }

        if (currentCytoscapeInstance) {
            currentCytoscapeInstance.layout(layoutOptions).run();
        }
    };


    panelDiv.appendChild(document.createElement('hr'));

    var layoutPersistenceContainer = document.createElement('div');
    layoutPersistenceContainer.style.marginTop = '10px';

    var saveBtn = document.createElement('button');
    saveBtn.innerText = 'Salvar Layout';
    saveBtn.id = 'exportLayoutBtn';
    saveBtn.style.width = '100%';
    saveBtn.style.marginBottom = '5px';


    var loadBtn = document.createElement('button');
    loadBtn.innerText = 'Carregar Layout';
    loadBtn.id = 'importLayoutBtn';
    loadBtn.style.width = '100%';
    
    var fileInput = document.createElement('input');
    fileInput.type = 'file';
    fileInput.id = 'layoutFileInput';
    fileInput.accept = '.json,application/json';
    fileInput.style.display = 'none';


    layoutPersistenceContainer.appendChild(saveBtn);
    layoutPersistenceContainer.appendChild(loadBtn);
    layoutPersistenceContainer.appendChild(fileInput); 

    panelDiv.appendChild(layoutPersistenceContainer);
}

async function loadDefaultLayoutsFromSeedFile() {
    const filePath = 'cy/all-cytoscape-layouts-backup.json';
    
    try {
        const response = await fetch(filePath);
        if (!response.ok) {
            throw new Error(`A resposta da rede não foi OK: ${response.statusText}`);
        }

        const allLayouts = await response.json();
        let layoutsLoaded = 0;

        for (const key in allLayouts) {
            if (key && key.startsWith('cyLayout_')) {
                const valueString = JSON.stringify(allLayouts[key]);
                localStorage.setItem(key, valueString);
                layoutsLoaded++;
            }
        }

        console.log(`${layoutsLoaded} layouts foram carregados com sucesso a partir do arquivo padrão.`);
        return true;

    } catch (error) {
        console.warn(`Não foi possível carregar o arquivo de layout padrão de '${filePath}'. Isso é esperado se você não forneceu um.`);
        return false;
    }
}

function hasExistingLayoutsInLocalStorage() {
    if (typeof localStorage === 'undefined') return false;

    for (let i = 0; i < localStorage.length; i++) {
        const key = localStorage.key(i);
        if (key && key.startsWith('cyLayout_')) {
            return true;
        }
    }
    return false;
}

function stopAutoDelay() {
    if (autoDelayTimer) {
        clearInterval(autoDelayTimer);
        autoDelayTimer = null;
    }
    var checkbox = document.getElementById('autoDelayCheckbox');
    if (checkbox) {
        checkbox.checked = false;
    }
}

function handleDelayClick() {
    stopAutoDelay();
    
    const delayInput = document.getElementById('autoDelayIntervalInput');
    let delayAmount = 1.0; 
    if (delayInput && delayInput.value) {
        let parsedValue = parseFloat(delayInput.value);
        if (!isNaN(parsedValue) && parsedValue > 0) {
            delayAmount = parsedValue;
        }
    }
    CaosConfig2.advanceTime(delayAmount);
}

function toggleAutoDelay(isChecked) {
    if (isChecked) {
        if (autoDelayTimer) return; 

        const getDelayAmount = () => {
            const delayInput = document.getElementById('autoDelayIntervalInput');
            let delaySeconds = 1.0; 
            if (delayInput && delayInput.value) {
                let parsedValue = parseFloat(delayInput.value);
                if (!isNaN(parsedValue) && parsedValue > 0) {
                    delaySeconds = parsedValue;
                } else {
                    delayInput.value = '1.0';
                }
            }
            return delaySeconds;
        };

        const runStep = () => {
            const delayAmount = getDelayAmount();
            CaosConfig2.advanceTime(delayAmount);
        };

        runStep(); 
        
        const delayInput = document.getElementById('autoDelayIntervalInput');
        let delayMilliseconds = getDelayAmount() * 1000;

        autoDelayTimer = setInterval(runStep, delayMilliseconds);

    } else {
        stopAutoDelay();
    }
}

function updateButtonHandlers(cy, graphId) {
    const saveBtn = document.getElementById('exportLayoutBtn');
    const loadBtn = document.getElementById('importLayoutBtn');
    const fileInput = document.getElementById('layoutFileInput');

    if (saveBtn) {
        saveBtn.onclick = () => exportAllLayoutsToFile(); 
    }
    
    if (loadBtn) {
        loadBtn.onclick = () => fileInput.click();
    }
    
    if (fileInput) {
        fileInput.onchange = (event) => {
            const file = event.target.files[0];
            if (!file) return;
            const reader = new FileReader();
            reader.onload = (e) => {
                importAllLayoutsFromFile(cy, graphId, e.target.result);
            };
            reader.readAsText(file);
            event.target.value = '';
        };
    }
}

function autoSaveLayoutToLocalStorage(cy, graphId) {
    if (!cy || !graphId || typeof localStorage === 'undefined') return;

    const positions = {};
    cy.nodes().forEach(node => {
        positions[node.id()] = node.position();
    });

    const storageKey = `cyLayout_${graphId}`;
    localStorage.setItem(storageKey, JSON.stringify(positions));
}


function loadLayoutFromLocalStorage(cy, graphId) {
    if (!cy || !graphId || typeof localStorage === 'undefined') return false;

    const storageKey = `cyLayout_${graphId}`;
    const savedLayout = localStorage.getItem(storageKey);

    if (savedLayout) {
        try {
            const positions = JSON.parse(savedLayout);
            cy.batch(() => {
                for (const nodeId in positions) {
                    const node = cy.getElementById(nodeId);
                    if (node.length > 0) node.position(positions[nodeId]);
                }
            });
            cy.fit(null, 50);
            console.log(`Layout carregado do LocalStorage para o grafo ${graphId.substring(0, 8)}...`);
            return true;
        } catch (e) {
            console.error("Falha ao carregar layout do LocalStorage:", e);
            localStorage.removeItem(storageKey);
            return false;
        }
    }
    return false;
}


function exportAllLayoutsToFile() {
    if (typeof localStorage === 'undefined') {
        alert("O LocalStorage não é suportado neste navegador.");
        return;
    }

    const allLayouts = {};
    let layoutsFound = 0;

    for (let i = 0; i < localStorage.length; i++) {
        const key = localStorage.key(i);

        if (key && key.startsWith('cyLayout_')) {

            allLayouts[key] = JSON.parse(localStorage.getItem(key));
            layoutsFound++;
        }
    }

    if (layoutsFound === 0) {
        alert("Nenhum layout salvo foi encontrado para exportar.");
        return;
    }

    const jsonString = JSON.stringify(allLayouts, null, 2);
    const blob = new Blob([jsonString], { type: 'application/json' });
    const a = document.createElement('a');
    a.href = URL.createObjectURL(blob);
    a.download = 'all-cytoscape-layouts-backup.json'; 
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(a.href);

    console.log(`${layoutsFound} layouts foram exportados com sucesso.`);
}


function importAllLayoutsFromFile(cy, graphId, jsonString) {
    if (typeof localStorage === 'undefined') {
        alert("O LocalStorage não é suportado neste navegador.");
        return;
    }

    try {
        const allLayouts = JSON.parse(jsonString);
        let layoutsImported = 0;


        for (const key in allLayouts) {
            if (key && key.startsWith('cyLayout_')) {
                const value = JSON.stringify(allLayouts[key]);
                localStorage.setItem(key, value);
                layoutsImported++;
            }
        }

        if (layoutsImported > 0) {
            alert(`${layoutsImported} layouts foram importados com sucesso para o seu navegador!`);
            
            console.log("Tentando aplicar o layout para o grafo atual...");
            loadLayoutFromLocalStorage(cy, graphId);

        } else {
            alert("Nenhum layout válido encontrado no arquivo selecionado.");
        }

    } catch (e) {
        console.error("Falha ao importar layouts do arquivo.", e);
        alert("Erro ao ler o arquivo. Verifique se é um backup de layout válido.");
    }
}




function generateGraphId(graphElements) {
    const stableString = JSON.stringify(graphElements); 
    
    let hash = 0;
    if (stableString.length === 0) {
        return '0';
    }
    for (let i = 0; i < stableString.length; i++) {
        const char = stableString.charCodeAt(i);
        hash = ((hash << 5) - hash) + char;
        hash = hash & hash; 
    }
    return String(hash);
}