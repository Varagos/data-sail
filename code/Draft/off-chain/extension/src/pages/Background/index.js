console.log('This is the background page.');
console.log('Put the background scripts here.');

let appState = {};

chrome.runtime.onMessage.addListener(function (request, sender, sendResponse) {
    console.log(`Background script received a message from ${sender.tab ? 'a content script:' + sender.tab.url : 'the extension'}`);
    console.log(request);
    if (request.type === 'NAMI_DETECTED') {
        console.log(`[Background script]: Received a message `, request);
        appState.namiDetected = request.payload;
    }
    // Handle other messages and update the state as needed
});
