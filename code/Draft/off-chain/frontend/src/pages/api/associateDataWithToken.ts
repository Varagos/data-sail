// pages/api/associateDataWithToken.js
import { NextApiRequest, NextApiResponse } from 'next';
import { storage } from '@/utilities/storage/index';

/**
 *  Runs when a data listing is made, removes data association from wallet and sets it to the DataToken
 * Add validation rule to ensure that the wallet address is the same as the one that has this token asset class
 */
const associateDataWithToken = async (req: NextApiRequest, res: NextApiResponse) => {
  if (req.method !== 'POST') {
    return res.status(405).end();
  }
  // console.log('Associate data with token', req.body);

  const walletAddr = req.body.walletAddr; // Validate and sanitize the data as needed
  const dataTokenAssetClass = req.body.tokenAssetClass;
  if (!dataTokenAssetClass || !walletAddr) {
    console.log('Missing params');
    return res.status(400).json({ error: 'DataTokenAssetClass and walletAddr are required' });
  }

  const cid = await storage.retrieveData(walletAddr);
  if (cid === null) {
    console.error('No data found for the given wallet address', walletAddr);
    return res.status(404).json({ success: false, message: 'No data found for the given wallet address' });
  }
  console.log('received cid:', cid);

  // These 2 would run in a transaction in a real system
  await storage.storeData(cid, dataTokenAssetClass);
  await storage.deleteData(walletAddr);

  res.status(201).json({ success: true });
};

export default associateDataWithToken;
