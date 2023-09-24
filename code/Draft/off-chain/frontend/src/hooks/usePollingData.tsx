import { AppStateContext } from '@/pages/_app';
import { DataSession } from '@/types';
import { useContext, useEffect, useState } from 'react';

const usePollingData = (interval: number) => {
  const [data, setData] = useState<DataSession>([]);

  const { appState } = useContext(AppStateContext);
  const { wAddr } = appState;

  useEffect(() => {
    const fetchData = async () => {
      try {
        if (!wAddr) {
          console.log('[usePollingData] No wallet address');
          return;
        }
        const response = await fetch(`/api/retrieveHistory?identifier=${wAddr}`);
        const data = await response.json();
        console.log('[usePollingData] Fetching data');

        console.log(`Fetching data`);
        console.log(data);
        setData(data.data);
      } catch (error) {
        console.log(`[usePollingData] Error fetching data`);
        console.log(error);
      }
    };

    fetchData(); // Initial fetch

    const intervalId = setInterval(fetchData, interval);

    return () => clearInterval(intervalId); // Cleanup on unmount
  }, [interval, wAddr]);

  return data;
};

export default usePollingData;
