import {
    Blockfrost,
    Lucid,
} from './modules/lucid-cardano';

(async function () {
    try {
        const cardanoExists = window.cardano !== undefined;
        const namiExists = window.cardano && window.cardano.nami !== undefined;

        if (!cardanoExists || !namiExists) {
            window.postMessage({ type: 'NAMI_DETECTED', payload: false }, '*');
        } else {
            // const nami = window.cardano.nami;

            // const api = await nami.enable()
            // console.log({ api })

            // console.log({ nami })
        }

        // if (window.cardano && window.cardano.nami) {
        //     document.dispatchEvent(new CustomEvent('NamiWalletDetected', { detail: { hasNamiWallet: true } }));
        // } else {
        //     document.dispatchEvent(new CustomEvent('NamiWalletDetected', { detail: { hasNamiWallet: false } }));
        // }

        // if (!window.cardano || !window.cardano.nami) {
        //     console.log(`[Injected]: Sending message to background script [NAMI_DETECTED]: true`);
        //     window.postMessage({ type: 'NAMI_DETECTED', payload: true }, '*');
        // } else {
        //     console.log(`[Injected]: Sending message to background script [NAMI_DETECTED]: false`);
        //     window.postMessage({ type: 'NAMI_DETECTED', payload: false }, '*');
        // }

        // const projectId = 'preview1qKQiEb3M4qLeDCkZQVlwMMzVI85bdox';
        // const blockfrost = new Blockfrost('https://cardano-preview.blockfrost.io/api/v0', projectId);

        // console.log("[Injected]: Blockfrost initialized", blockfrost);

        // const lucid = await Lucid.new(blockfrost, 'Preview');
        // console.log(`[Injected]: Lucid`, lucid);
        // const nami = await window.cardano.nami.enable();
    } catch (error) {
        console.error(`[Injected]: Error initializing Lucid`, error);
    }
})();
