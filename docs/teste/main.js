var currentCytoscapeInstance = null;


function renderCytoscapeGraph(mainContainerId, combinedJsonData, isFirstRender) {
    var mainContainer = document.getElementById(mainContainerId);
    if (!mainContainer) {
        console.error('Main container not found: ' + mainContainerId);
        return;
    }

    try {
        var data = JSON.parse(combinedJsonData);

        if (!isFirstRender && currentCytoscapeInstance) {
            updateSidePanel(mainContainerId + '_panel', data.panelData);
            
            currentCytoscapeInstance.json({ elements: data.graphElements });
            
            if (data.lastTransition) {
                var trans = data.lastTransition;
                var from = trans.from;
                var to = trans.to;
                var lbl = trans.lbl;
                var elementsToFlash;

                if (lbl === "") {
                    var edgeId = `#direct_${from}_${to}_`;
                    elementsToFlash = currentCytoscapeInstance.getElementById(edgeId);
                } else { 
                    var eventNodeId = `${from}_${to}_${lbl}`;
                    var eventNodeSelector = `#${eventNodeId}`;
                    var edgeToEventSelector = `#conn_s_${from}_${eventNodeId}`;
                    var edgeFromEventSelector = `#conn_t_${eventNodeId}_${to}`;
                    
                    elementsToFlash = currentCytoscapeInstance.elements(`${eventNodeSelector}, ${edgeToEventSelector}, ${edgeFromEventSelector}`);
                }

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
                { selector: 'edge', style: { 'width': 2, 'curve-style': 'bezier', 'target-arrow-shape': 'none', 'line-color': '#565f89', 'target-arrow-color': '#565f89', 'line-style': 'solid' } },
                { selector: '.from-event-node', style: { 'target-arrow-shape': 'triangle' } },
                { selector: '.disabled', style: { 'line-style': 'dashed', 'line-color': '#414868', 'target-arrow-color': '#414868' } },
                { selector: 'edge[label]', style: { 'label': 'data(label)', 'font-size': '12px', 'color': '#c0caf5', 'text-background-color': '#24283b', 'text-background-opacity': 1, 'text-background-padding': '3px', 'text-background-shape': 'round-rectangle', 'text-rotation': 'autorotate', 'text-margin-y': -15 } },
                { selector: '.state-node', style: { 'background-color': '#7aa2f7', 'shape': 'ellipse' } },
                { selector: '.current-state', style: { 'background-color': '#9ece6a', 'border-color': '#c0caf5', 'border-width': 3, 'border-style': 'solid' } },
                { selector: '.event-node', style: { 'background-color': '#565f89', 'shape': 'rectangle', 'width': '25px', 'height': '25px' } },
                { selector: '.event-node.enabled', style: { 'background-color': '#ff9e64' } },
                { selector: '.event-node.disabled', style: { 'background-color': '#565f89', 'opacity': 0.7 } },
                { selector: '.compound-parent', style: { 'background-color': '#24283b', 'background-opacity': 0.5, 'border-color': '#414868', 'border-width': 2, 'content': 'data(label)', 'text-valign': 'top', 'color': '#c0caf5', 'text-outline-width': 0, 'font-weight': 'bold', 'font-size': '16px' } },
                
                { selector: '.rule-edge', style: { 'width': 1.5, 'source-arrow-shape': 'circle' } },
                { selector: '.enable-rule',
                     style: { 
                        'line-color': '#7aa2f7', 
                        'target-arrow-color': '#7aa2f7', 
                        'source-arrow-color': '#7aa2f7', 
                        'color': '#7aa2f7', 
                        'target-arrow-shape': 'circle-triangle', 
                        'font-size': '12px',
                        'target-text-offset': 5,
                        
                     } },
                
                { 
                    selector: '.disable-rule', 
                    style: { 
                        'line-color': '#f7768e',
                        'source-arrow-color': '#f7768e',
                        'target-label': 'X', 
                        'target-arrow-shape': 'none',
                        'font-size': '12px',
                        'color': '#f7768e',
                        'target-text-offset': 5
                    } 
                },
                
                { selector: '.spontaneous-transition', style: {'line-color': '#7dcfff', 'target-arrow-color': '#7dcfff','line-style': 'dotted'}},
                { selector: 'node.transition-flash',style: {'background-color': '#ff9e64','border-color': 'white' }},
                { selector: 'edge.transition-flash',style: {'line-color': '#ff9e64','target-arrow-color': '#ff9e64','source-arrow-color': '#ff9e64','width': 4}},
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
}