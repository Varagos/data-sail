import { DataSession } from '@/types';
import { useEffect, useState } from 'react';

const usePollingData = (interval: number) => {
  const [data, setData] = useState<DataSession[]>([]);

  useEffect(() => {
    const fetchData = async () => {
      try {
        const response = await fetch(`/api/retrieveHistory`);
        const data = await response.json();

        // console.log(`Fetching data`);
        // console.log(data);
        const success = data.success;
        setData(data.data);
      } catch (error) {
        console.log(`Fetching data`);
        console.log(error);
      }
    };

    fetchData(); // Initial fetch

    const intervalId = setInterval(fetchData, interval);

    return () => clearInterval(intervalId); // Cleanup on unmount
  }, [interval]);

  return data;
};

export default usePollingData;
