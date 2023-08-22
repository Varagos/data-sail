// pages/api/saveHistory.js
import { storage } from '@/utilities/storage';

const retrieveHistory = async (req, res) => {
  if (req.method !== 'GET') {
    return res.status(405).end();
  }

  const result = await storage.retrieveAllData();
  console.log('result', result);
  res.status(200).json({ success: true, data: result });
};

export default retrieveHistory;
