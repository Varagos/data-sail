// pages/api/saveHistory.js
import { storage } from '@/utilities/storage/index';

const retrieveHistory = async (req: any, res: any) => {
  if (req.method !== 'GET') {
    return res.status(405).end();
  }

  const result = await storage.retrieveAllData();
  // console.log('Retrieving history result', result);
  // console.log('Retrieving history result');
  res.status(200).json({ success: true, data: result });
};

export default retrieveHistory;
