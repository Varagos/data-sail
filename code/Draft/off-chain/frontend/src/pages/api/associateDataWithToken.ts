// pages/api/associateDataWithToken.js
import { NextApiRequest, NextApiResponse } from 'next';
import { storage } from '@/utilities/storage/index';
import { tokenListingStorageService } from '@/services/token-listings';

/**
 * Runs when a token is minted, removes data association from wallet and sets it to the DataToken
 * Add validation rule to ensure that the wallet address is the same as the one that has this token asset class(with digital signature)
 *
 * (OR ELSE-better) The server can just verify that the dataTokenAssetClass is owned by the walletAddr
 * This wallet is the only one that can alter the data associated with the token, so if it owns the token, it can associate the data with it
 * Basically this acts like off-chain metadata for the token
 */
const associateDataWithToken = async (req: NextApiRequest, res: NextApiResponse) => {
  if (req.method !== 'POST') {
    return res.status(405).end();
  }

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

  // Adds token listing to the database, so any bidders can find all available tokens
  await tokenListingStorageService.addTokenListing({
    owner: walletAddr,
    tokenAssetClass: dataTokenAssetClass,
  });

  res.status(201).json({ success: true });
};

export default associateDataWithToken;
