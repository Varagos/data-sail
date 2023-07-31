import React from 'react';
import ReactDOM from 'react-dom/client';
import './index.css';
import './styles/globals.css';
import App from './App';
import AppStateProvider from './context/AppStateContext';

const root = document.createElement('div');
/**
 * bg-zinc-50: This sets the background color of your container (replace zinc-50 with your preferred color and shade).

rounded-lg: This gives your container rounded corners.

shadow-lg: This gives your container a large shadow.

p-6: This adds padding inside your container.

flex flex-col items-center: These classes set up a flex container in the column direction with items centered horizontally.

font-quicksand text-zinc-800: These set the font and text color.
 */
root.className =
  'w-96 h-auto bg-zinc-50 rounded-lg shadow-lg p-6 flex flex-col items-center font-quicksand text-zinc-800';

document.body.appendChild(root);
const rootDiv = ReactDOM.createRoot(root);
rootDiv.render(
  <React.StrictMode>
    <AppStateProvider>
      <App />
    </AppStateProvider>
  </React.StrictMode>
);
