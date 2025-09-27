var currentCytoscapeInstance = null;
var textTraceHistory = [];

function renderCytoscapeGraph(mainContainerId, combinedJsonData, isFirstRender) {
    var mainContainer = document.getElementById(mainContainerId);
    if (!mainContainer) {
        console.error('Main container not found: ' + mainContainerId);
        return;
    }

    try {
        var data = JSON.parse(combinedJsonData);
        console.log(data);
        if (isFirstRender){
            textTraceHistory = [];
            const target = data.graphElements.find(
            el => el.classes === "state-node current-state"
            );
            textTraceHistory.push(target.data.id);
            
        }
        
        if (!isFirstRender && currentCytoscapeInstance) {
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
                    }, 500); 
                }
            }
            
            return; 
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
                { selector: 'edge', style: { 'width': 2, 'curve-style': 'bezier', 'target-arrow-shape': 'triangle', 'line-color': '#565f89', 'target-arrow-color': '#565f89' } },

                { selector: 'node.state-node', style: { 'background-color': '#7aa2f7', 'shape': 'ellipse', 'width': 50, 'height': 50, 'border-width': 3, 'border-color': '#414868' } },
                { selector: '.current-state', style: { 'background-color': '#9ece6a', 'border-color': '#c0caf5' } },

                { selector: 'node.event-node', style: { 'background-color': '#414868', 'shape': 'rectangle', 'width': 50, 'height': 30, 'border-width': 2, 'border-color': '#565f89' } },
                
                { selector: 'edge.rule-edge', style: { 'target-arrow-shape': 'none' } },
                
                { selector: '.enable-rule', style: { 'line-color': '#7aa2f7', 'target-arrow-color': '#7aa2f7' } },
                { selector: 'edge.enable-rule.to-target', style: { 'target-arrow-shape': 'triangle' } },

                { selector: '.disable-rule', style: { 'line-color': '#f7768e', 'target-arrow-color': '#f7768e' } },
                { selector: 'edge.disable-rule.to-target', style: { 'target-label': 'X', 'target-text-offset': 5, 'color': '#f7768e' } },

                { selector: '.disabled', style: { 'line-style': 'dashed', 'background-opacity': 0.6, 'border-style': 'dashed', 'opacity': 0.7 } },
                
                { selector: 'node.transition-flash', style: {'background-color': '#ff9e64','border-color': 'white' }},
                { selector: 'edge.transition-flash', style: {'line-color': '#ff9e64','target-arrow-color': '#ff9e64','source-arrow-color': '#ff9e64','width': 4}},
                { selector: '.trace-path', style: { 'line-color': '#7dcfff', 'target-arrow-color': '#7dcfff', 'source-arrow-color': '#7dcfff', 'width': 3.5, 'opacity': 0.9 } },
                { selector: '.compound-parent', style: { 'background-color': '#1a1b26', 'background-opacity': 1,      'border-color': '#c0caf5',   'border-width': 2,'content': 'data(label)', 'text-valign': 'top','text-halign': 'center','color': '#c0caf5','font-weight': 'bold','font-size': '16px'} },
            ],
            layout: {
                name: 'dagre',
                rankDir: 'LR', 
                fit: true,
                padding: 50, 
                spacingFactor: 1.2, 
                nodeSep: 60,       
                rankSep: 70,     
                edgeSep: 10         
            }
        });

        cy.on('tap', 'node.event-node.enabled', function(evt){
            var node = evt.target;
            var nodeId = node.id();
            
            var parts = nodeId.split('_');
            if (parts.length >= 3) {
                var from = parts[0];
                var to = parts[1];
                var lbl = parts.slice(2).join('_');
                var edgeJson = JSON.stringify({ "from": from, "to": to, "lbl": lbl });
                CaosConfig2.takeStep(edgeJson);
            }
        });
        
        cy.on('mouseover', 'node.event-node.enabled', function(e) {
            var container = e.cy.container();
            container.style.cursor = 'pointer';
        });
        cy.on('mouseout', 'node.event-node.enabled', function(e) {
            var container = e.cy.container();
            container.style.cursor = 'default';
        });

        currentCytoscapeInstance = cy;

    } catch (e) {
        console.error("Failed to parse or render UI:", e);
        if (mainContainer) {
            mainContainer.innerText = "Error building UI. Check console for details.";
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
    layoutSelectorContainer.style.marginBottom = '10px';

    var layoutLabel = document.createElement('label');
    layoutLabel.innerText = 'Layout: ';
    layoutLabel.htmlFor = 'layoutSelector';

    var layoutSelector = document.createElement('select');
    layoutSelector.id = 'layoutSelector';
    layoutSelector.innerHTML = `
        <option value="dagre" selected>Dagre (Hierárquico)</option>
        <option value="cose">Cose (Forças)</option>
    `;

    layoutSelectorContainer.appendChild(layoutLabel);
    layoutSelectorContainer.appendChild(layoutSelector);
    panelDiv.appendChild(layoutSelectorContainer); 

    layoutSelector.onchange = function(event) {
        var layoutName = event.target.value;
        var layoutOptions;
        if (layoutName === 'dagre') {
            layoutOptions = { name: 'dagre', rankDir: 'LR', fit: true, padding: 50, spacingFactor: 1.2, nodeSep: 60, rankSep: 70, edgeSep: 10 };
        } else if (layoutName === 'cose') {
            layoutOptions = { name: 'cose', animate: true, fit: true, padding: 30, nodeRepulsion: 400000, idealEdgeLength: 100, componentSpacing: 100 };
        }

        if (currentCytoscapeInstance) {
            currentCytoscapeInstance.layout(layoutOptions).run();
        }
    };
}