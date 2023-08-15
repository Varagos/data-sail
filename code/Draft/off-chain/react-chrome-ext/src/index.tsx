import React from 'react';
import ReactDOM from 'react-dom/client';
import './index.css';
import './styles/globals.css';
import App from './App';
import AppStateProvider from './context/AppStateContext';

const root = document.createElement('div');

document.body.appendChild(root);
const rootDiv = ReactDOM.createRoot(root);
rootDiv.render(
  <React.StrictMode>
    <AppStateProvider>
      <iframe id="mySandbox" src="sandbox.html" style={{ display: 'none' }}></iframe>

      <App />
    </AppStateProvider>
  </React.StrictMode>
);
