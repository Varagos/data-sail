// Function to truncate text in the middle
export const truncateMiddle = (text: string, startChars = 6, endChars = 6, separator = '...') => {
  if (text.length <= startChars + endChars) {
    return text;
  }
  return text.substr(0, startChars) + separator + text.substr(text.length - endChars);
};
