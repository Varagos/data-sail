// pages/api/saveHistory.js
import { storage } from '@/utilities/storage/index';

const saveHistory = async (req, res) => {
  if (req.method !== 'POST') {
    return res.status(405).end();
  }

  console.log('req.body', req.body);
  const data = req.body.data; // Validate and sanitize the data as needed
  const result = await storage.storeData(data);
  console.log('Saving history result', result);

  res.status(201).json({ success: true, data: result });
};

export default saveHistory;
