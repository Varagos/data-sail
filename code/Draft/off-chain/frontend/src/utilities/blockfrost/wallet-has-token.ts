import { BlockFrostAPI } from '@blockfrost/blockfrost-js';

const project_id = process.env.BLOCKFROST_API_KEY; // Using env variable here

if (!project_id) {
  throw new Error('BLOCKFROST_API_KEY environment variable not set');
}
const api = new BlockFrostAPI({ projectId: project_id });
export const walletHasToken = async (walletAddress: string, tokenAssetClass: string): Promise<boolean> => {
  console.log('assetClass', tokenAssetClass);
  const res = await api.addressesUtxosAsset(walletAddress, tokenAssetClass);
  return res.length === 1;
};
