// pages/api/associateDataWithToken.js
import { NextApiRequest, NextApiResponse } from 'next';
import { storage } from '@/utilities/storage/index';
import { redisTokenListings } from '@/services/token-listings';

/**
 * The server can just verify that the dataTokenAssetClass is owned by the walletAddr
 * This wallet is the only one that can alter the data associated with the token, so if it owns the token, it can associate the data with it
 * Basically this acts like off-chain metadata for the token
 */
const associateDataWithToken = async (req: NextApiRequest, res: NextApiResponse) => {
  if (req.method !== 'POST') {
    return res.status(405).end();
  }

  console.info('POST /api/associateDataWithToken');

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

  await redisTokenListings.addTokenListing({
    owner: walletAddr,
    tokenAssetClass: dataTokenAssetClass,
  });

  res.status(201).json({ success: true });
};

export default associateDataWithToken;
