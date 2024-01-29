import { AppStateContext } from '@/pages/_app';
import { DataSession } from '@/types';
import { createDigitalSignatureHeader, createWalletHeader } from '@/utilities/digital-signature/create-header';
import { signMessage } from '@/utilities/digital-signature/sign-message';
import { useContext, useEffect, useState } from 'react';

export type WalletData = {
  data: DataSession;
  cid: string;
};

const useFetchWalletData = (): [WalletData | null, () => Promise<void>] => {
  const [data, setData] = useState<WalletData | null>(null);

  const { appState } = useContext(AppStateContext);
  const { wAddr, lucid } = appState;

  const fetchData = async () => {
    try {
      if (!wAddr) {
        console.log('[useFetchWalletData] No wallet address');
        return;
      }
      if (!lucid) {
        console.log('[useFetchWalletData] No lucid');
        return;
      }
      const payload = 'Fetch_wallet_data_' + Date.now();
      const signedMessage = await signMessage(lucid, payload, wAddr);
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
        // console.log(`[useFetchWalletData] Error fetching data: ${data.message}`);
        return;
      }

      setData({
        data: data.data,
        cid: data.cid,
      });
    } catch (error) {
      console.error(error);
      console.log(`[useFetchWalletData] Error fetching data`);
    }
  };
  // useEffect(() => {
  //   fetchData(); // Initial fetch
  // }, [wAddr, lucid]);

  return [data, fetchData];
};

export default useFetchWalletData;
