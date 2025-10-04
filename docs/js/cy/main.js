var currentCytoscapeInstance = null;
var textTraceHistory = [];




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


function setupInitialCytoscape(mainContainerId, combinedJsonData) {
    var mainContainer = document.getElementById(mainContainerId);

    try {
        var data = JSON.parse(combinedJsonData);
        
        textTraceHistory = [];
        const target = data.graphElements.find(
            el => el.classes && el.classes.includes("current-state")
        );
        if (target) {
            textTraceHistory.push(target.data.id);
        }

        const mermaidNodePositions = extractMermaidPositions('id-1996100447Box');
        const mermaidLabelPositions = extractMermaidEdgeLabelPositions('id-1996100447Box');
        
        let layoutName = 'dagre'; 

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
            //console.log(idMap);
            //console.log(labelMap);
            //console.log(data.graphElements);
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
                { selector: 'edge', style: { 'width': 2, 'curve-style': 'bezier', 'line-color': '#565f89', 'target-arrow-color': '#565f89', 'label': 'data(label)', 'color': '#c0caf5', 'text-outline-color': '#1a1b26', 'text-outline-width': 2, 'font-size': '8px' } },
                { selector: 'node.state-node', style: { 'background-color': '#7aa2f7', 'shape': 'ellipse', 'width': 50, 'height': 50, 'border-width': 3, 'border-color': '#414868' } },
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
            ],
            layout: {
                name: layoutName,
                rankDir: 'LR', fit: true, padding: 50, spacingFactor: 1.2, nodeSep: 60, rankSep: 70, edgeSep: 10
            }
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

        currentCytoscapeInstance = cy;

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
            transButton.onclick = function() {
                CaosConfig2.takeStep(JSON.stringify(edge));
            };
            panelDiv.appendChild(transButton);
        });
    }

    var layoutSelectorContainer = document.createElement('div');
    layoutSelectorContainer.style.marginTop = '20px';
    var layoutLabel = document.createElement('label');
    layoutLabel.innerText = 'Layout: ';
    layoutLabel.htmlFor = 'layoutSelector';
    var layoutSelector = document.createElement('select');
    layoutSelector.id = 'layoutSelector';
    layoutSelector.innerHTML = `<option value="preset">Sincronizado</option><option value="dagre">Dagre (Hierárquico)</option><option value="cose">Cose (Forças)</option>`;
    layoutSelectorContainer.appendChild(layoutLabel);
    layoutSelectorContainer.appendChild(layoutSelector);
    panelDiv.appendChild(layoutSelectorContainer); 

    layoutSelector.onchange = function(event) {
        var layoutName = event.target.value;
        var layoutOptions;
        if (layoutName === 'dagre') {
            layoutOptions = { name: 'dagre', rankDir: 'LR', fit: true, padding: 50, spacingFactor: 1.2, nodeSep: 60, rankSep: 70, edgeSep: 10 };
        } else if (layoutName === 'cose') {
            layoutOptions = { name: 'cose', animate: true, fit: true, padding: 30, nodeRepulsion: 9000, idealEdgeLength: 100, componentSpacing: 100 };
        } else {
             layoutOptions = { name: 'preset', fit: true, padding: 30 }; 
        }

        if (currentCytoscapeInstance) {
            currentCytoscapeInstance.layout(layoutOptions).run();
        }
    };
}