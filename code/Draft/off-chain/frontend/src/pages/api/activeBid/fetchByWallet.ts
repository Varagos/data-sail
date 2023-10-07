import { activeBidsStorageService } from '@/services/active-bids';
import type { NextApiRequest, NextApiResponse } from 'next';

export default async function handler(req: NextApiRequest, res: NextApiResponse) {
  if (req.method !== 'GET') {
    return res.status(405).end();
  }
  const { wallet } = req.query;

  try {
    const bids = await activeBidsStorageService.fetchAllActiveBids(wallet as string);

    res.status(200).json({ data: bids });
  } catch (error) {
    console.error(error);
    res.status(500).json({ error: 'Internal server error' });
  }
}
