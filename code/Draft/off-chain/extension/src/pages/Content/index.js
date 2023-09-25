import { printLine } from './modules/print';

console.log('Content script works!');
console.log('Must reload extension for modifications to take effect.');

printLine("Using the 'printLine' function from the Print Module");


// const cardanoExists = !!window.cardano;
// console.log('content.js: cardanoExists', cardanoExists);

// content-script.js
// if (window.cardano && window.cardano.nami) {
//     chrome.runtime.sendMessage({ hasNamiWallet: true });
// } else {
//     chrome.runtime.sendMessage({ hasNamiWallet: false });
// }
function checkForCardano() {
    const cardanoExists = window.cardano !== undefined;
    console.log('content.js: cardanoExists', cardanoExists);

    //   if (window.cardano && window.cardano.nami) {
    //     chrome.runtime.sendMessage({ hasNamiWallet: true });
    //   } else {
    //     chrome.runtime.sendMessage({ hasNamiWallet: false });
    //   }
}

// Check for cardano every second
checkForCardano()

// You might want to clear the interval after some time to avoid unnecessary checks

console.log(`[Content script]: Injecting injected.bundle.js`);
const script = document.createElement('script');
script.src = chrome.runtime.getURL('injected.bundle.js');
(document.head || document.documentElement).appendChild(script);


window.addEventListener('message', function (event) {
    // In general we receive a tons of events, even from other extensions, the page itself, etc.
    if (event.source !== window) return;
    if (event.data.type && event.data.type === 'NAMI_DETECTED') {
        console.log(`[Content script]: Received a message `, event);
        chrome.runtime.sendMessage(event.data);
    }
});
