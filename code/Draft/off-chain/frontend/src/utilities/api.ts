async function sendDataToDecrypt() {
  const encryptedData = 'your_encrypted_data_here';
  const res = await fetch('/api/retrieveHistoryForBuyer', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({ encryptedData }),
  });

  if (res.ok) {
    const { decryptedData } = await res.json();
    console.log('Decrypted Data:', decryptedData);
  } else {
    console.log('Error:', res.status);
  }
}

async function sendDataToEncrypt() {
  const data = 'some_data_to_encrypt';
  const res = await fetch('/api/encrypt', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({ data }),
  });

  if (res.ok) {
    const { encryptedData } = await res.json();
    console.log('Encrypted Data:', encryptedData);
  } else {
    console.log('Error:', res.status);
  }
}

export async function associateDataWithToken(walletAddr: string, dataTokenAssetClass: string) {
  const res = await fetch('/api/associateDataWithToken', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({ walletAddr, tokenAssetClass: dataTokenAssetClass }),
  });

  if (res.ok) {
    console.log('Associated data with token');
  } else {
    console.log('Error:', res.status);
  }
}
