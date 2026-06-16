function parseCookies(cookieString) {
  const cookies = cookieString.split("; ");
  const cookiesDict = {};
  for (const cookie of cookies) {
    const [key, value] = cookie.split("=", 2);
    cookiesDict[key] = value;
  }
  return cookiesDict;
}

function sendCanvasToPrinter(canvas, text) {
  const csrf = parseCookies(document.cookie).receipt_csrf;
  if (!csrf) {
    console.error("You are not logged into Receipt API Server! Aborting...");
    return;
  }
  canvas.toBlob((blob) => {
    fetch("https://receipt.recurse.com/text", {
      method: "POST",
      credentials: "include",
      headers: {
        "Content-Type": "application/json",
        "X-CSRF-Token": csrf,
      },
      body: JSON.stringify({
        text,
        coda: "none",
      }),
    });
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

function p(s) {
  const el = document.createElement("p");
  el.innerHTML = s;
  return el;
}

function a(s, url) {
  const el = document.createElement("a");
  el.innerHTML = s;
  el.src = url;
  return el;
}
function setupLoginSection() {
  const csrf = parseCookies(document.cookie).receipt_csrf;
  if (!window.location.origin.match(/.recurse.com\/?$/)) {
    loginDiv.appendChild(
      p(
        'This is not a *.recurse.com subdomain, so it will not be able to make authenticated requests to <a href="https://receipt.recurse.com">https://receipt.recurse.com</a>.',
      ),
    );
    loginDiv.appendChild(
      p(
        'Please visit <a href="https://receipt-tester.recurse.com">https://receipt-tester.recurse.com</a> for the full experience.',
      ),
    );
    return;
  }
  if (csrf) {
    loginDiv.appendChild(p("You are logged in."));
    return;
  }
  loginDiv.appendChild(
    a(
      "login",
      `https://receipt.recurse.com/login?redirect_uri=${window.location.href}`,
    ),
  );
}
