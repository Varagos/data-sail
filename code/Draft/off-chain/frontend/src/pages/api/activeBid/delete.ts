import { redisActiveBids } from '@/services/active-bids';
import type { NextApiRequest, NextApiResponse } from 'next';

export default async function handler(req: NextApiRequest, res: NextApiResponse) {
  if (req.method !== 'DELETE') {
    return res.status(405).end();
  }
  console.info('DELETE /api/activeBid/delete');
  const { wallet, bidId } = req.body;

  try {
    await redisActiveBids.removeActiveBid(wallet as string, bidId as string);
    res.status(200).json({ message: `Deleted active bid with ID ${bidId}` });
  } catch (error) {
    console.error(error);
    res.status(500).json({ error: 'Internal server error' });
  }
}
