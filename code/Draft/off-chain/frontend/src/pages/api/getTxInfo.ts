import { NextApiRequest, NextApiResponse } from 'next';
import { BlockFrostAPI } from '@blockfrost/blockfrost-js';

const project_id = process.env.BLOCKFROST_API_KEY; // Using env variable here

if (!project_id) {
  throw new Error('BLOCKFROST_API_KEY environment variable not set');
}
const api = new BlockFrostAPI({ projectId: project_id });

export default async function handler(req: NextApiRequest, res: NextApiResponse) {
  const txHash = req.query.txHash as string;

  if (!txHash) {
    return res.status(400).json({ error: 'Missing transaction hash' });
  }

  try {
    const txUtxos = await api.txsUtxos(txHash);
    // console.log(txUtxos);
    const inputAddress = txUtxos.inputs.map((i) => i.address);
    const uniqueInputAddress = [...new Set(inputAddress)];
    return res.status(200).json({ inputAddress: uniqueInputAddress });

    // return res.status(200).json(txInfo);
  } catch (error) {
    return res.status(500).json({ error: 'An error occurred while fetching the transaction' });
  }
}
