function parseCookies(cookieString) {
  const cookies = cookieString.split("; ");
  const cookiesDict = {};
  for (const cookie of cookies) {
    const [key, value] = cookie.split("=", 2);
    cookiesDict[key] = value;
  }
  return cookiesDict;
}

function sendCanvasToPrinter(canvas) {
  const csrf = parseCookies(document.cookie).receipt_csrf;
  if (!csrf) {
    console.error("You are not logged into Receipt API Server! Aborting...");
    return;
  }
  canvas.toBlob(() => {
    fetch("https://receipt.recurse.com/image", {
      method: "POST",
      credentials: "include",
      headers: {
        "Content-Type": "application/octet-stream",
        "X-CSRF-Token": csrf,
      },
      body: blob,
    });
  }, "image/png");
}
