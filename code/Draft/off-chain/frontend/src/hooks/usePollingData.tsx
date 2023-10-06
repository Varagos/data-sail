import { AppStateContext } from '@/pages/_app';
import { DataSession } from '@/types';
import { createDigitalSignatureHeader, createWalletHeader } from '@/utilities/digital-signature/create-header';
import { signMessage } from '@/utilities/digital-signature/sign-message';
import { useContext, useEffect, useState } from 'react';

const usePollingData = (interval: number) => {
  const [data, setData] = useState<DataSession>([]);
  const [waitingForSignature, setWaitingForSignature] = useState(false);

  const { appState } = useContext(AppStateContext);
  const { wAddr, lucid } = appState;

  useEffect(() => {
    const fetchData = async () => {
      try {
        if (!wAddr) {
          console.log('[usePollingData] No wallet address');
          return;
        }
        if (!lucid) {
          console.log('[usePollingData] No lucid');
          return;
        }
        if (waitingForSignature) {
          console.log('[usePollingData] Waiting for signature');
          return;
        }
        const payload = 'Test' + Date.now();
        setWaitingForSignature(true);
        const signedMessage = await signMessage(lucid, payload, wAddr);
        setWaitingForSignature(false);
        const [signatureKey, signatureValue] = createDigitalSignatureHeader(payload, signedMessage);
        const [walletKey, walletValue] = createWalletHeader(wAddr);

        const headers = new Headers();
        headers.append(signatureKey, signatureValue);
        headers.append(walletKey, walletValue);
        const response = await fetch(`/api/retrieveHistory?identifier=${wAddr}`, {
          method: 'GET',
          headers,
        });
        const data = await response.json();
        if (!response.ok) {
          // Probably 404 - no data found
          // console.log(`[usePollingData] Error fetching data: ${data.message}`);
          return;
        }
        // console.log('[usePollingData] Fetching data');

        // console.log(`Fetching data`);
        // console.log(data);
        setData(data.data);
      } catch (error) {
        console.log(`[usePollingData] Error fetching data`);
        // console.log(error);
      }
    };

    fetchData(); // Initial fetch

    const intervalId = setInterval(fetchData, interval);

    return () => clearInterval(intervalId); // Cleanup on unmount
  }, [interval, wAddr, lucid]);

  return data;
};

export default usePollingData;
