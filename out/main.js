window.onload = function() {
  setupLoginSection()
  setupTextSection()
  setupImageSection()
  setupEscPosSection()
}

function setupEscPosSection() {
  if (!getReceiptCsrfCookie()) {
    sendEscPosButton.disabled = true
  }
  sendEscPosButton.addEventListener('click', async (event) => {
    sendEscPosButton.disabled = true
    initResponseDiv(escPosResponseDiv, 'Response: (pending...)')
    // TODO
    initResponseDiv(escPosResponseDiv, 'Response:')
    updateResponseDiv(escPosResponseDiv, 999, '{}')
    sendEscPosButton.disabled = false
  })
}

function setupImageSection() {
  if (!getReceiptCsrfCookie()) {
    printImageButton.disabled = true
    imagePicker.disabled = true
  }
  imagePicker.addEventListener('change', async () => {
    printImageButton.disabled = imagePicker.files.length !== 1
  })
  printImageButton.addEventListener('click', async (event) => {
    printImageButton.disabled = true
    initResponseDiv(imageResponseDiv, 'Response: (pending...)')
    const res = await sendImageToPrinter()
    const jsonBody = await res.json()
    initResponseDiv(imageResponseDiv, 'Response:')
    updateResponseDiv(imageResponseDiv, res.status, jsonBody)
    printImageButton.disabled = false
  })
}

function clear(node) {
  while (node.firstChild) {
    node.removeChild(node.lastChild)
  }
}

function initResponseDiv(div, label) {
  clear(div)
  div.appendChild(p(label))
  for (let i = 1; i < div.children.length; i++) {
    div.removeChild(div.children[i])
  }
}

function updateResponseDiv(div, status, jsonResponse) {
  div.appendChild(pre(status))
  div.appendChild(pre(JSON.stringify(jsonResponse, null, 2)))
}

function sendImageToPrinter() {
  const csrf = getReceiptCsrfCookie()
  if (!csrf) {
    console.error('You are not logged into Receipt API Server! Aborting...')
    return
  }
  if (imagePicker.files.length === 0) {
    console.error('No image loaded!')
    return
  }
  return fetch('https://receipt.recurse.com/image', {
    method: 'POST',
    credentials: 'include',
    headers: {
      'Content-Type': 'application/octet-stream',
      'X-CSRF-Token': csrf,
    },
    body: imagePicker.files[0],
  })
}

function setupTextSection() {
  if (!getReceiptCsrfCookie()) {
    printTextButton.disabled = true
  }
  printTextButton.addEventListener('click', async (event) => {
    printTextButton.disabled = true
    const text = textField.value
    setLastPrinted(text)
    initResponseDiv(textResponseDiv, 'Response: (pending...)')
    const res = await sendTextToPrinter(text)
    const jsonBody = await res.json()
    initResponseDiv(textResponseDiv, 'Response:')
    updateResponseDiv(textResponseDiv, res.status, jsonBody)
    textField.innerHTML = ''
    printTextButton.disabled = false
  })
}

function sendTextToPrinter(text) {
  const csrf = getReceiptCsrfCookie()
  if (!csrf) {
    console.error('You are not logged into Receipt API Server! Aborting...')
    return
  }
  return fetch('https://receipt.recurse.com/text', {
    method: 'POST',
    credentials: 'include',
    headers: {
      'Content-Type': 'application/json',
      'X-CSRF-Token': csrf,
    },
    body: JSON.stringify({
      text
    })
  })
}

function setLastPrinted(text) {
  if (lastPrintedDiv.children.length === 0) {
    lastPrintedDiv.appendChild(p('Sent:'))
  }
  for (let i = 1; i < lastPrintedDiv.children.length; i++) {
    lastPrintedDiv.removeChild(lastPrintedDiv.children[i])
  }
  lastPrintedDiv.appendChild(pre(text))
}

function parseCookies(cookieString) {
  const cookies = cookieString.split('; ')
  const cookiesDict = {}
  for (const cookie of cookies) {
    const [key, value] = cookie.split('=', 2)
    cookiesDict[key] = value
  }
  return cookiesDict
}

function a(text, url) {
  const el = document.createElement('a')
  el.innerHTML = text
  el.href = url
  return el
}

function pre(text) {
  const el = document.createElement('pre')
  el.innerHTML = text
  return el
}

function p(text) {
  const el = document.createElement('p')
  el.innerHTML = text
  return el
}

function setupLoginSection() {
  if (!window.location.origin.match(/.recurse.com\/?$/)) {
    loginDiv.appendChild(p('This is not a *.recurse.com subdomain, so it will not be able to make authenticated requests to <a href="https://receipt.recurse.com">https://receipt.recurse.com</a>.'))
    loginDiv.appendChild(p('Please visit <a href="https://receipt-tester.recurse.com">https://receipt-tester.recurse.com</a> for the full experience.'))
    return
  }
  if (getReceiptCsrfCookie()) {
    loginDiv.appendChild(p('You are logged in.'))
    return
  }
  loginDiv.appendChild(a('login', `https://receipt.recurse.com/login?redirect_uri=${window.location.href}`))
}

function getReceiptCsrfCookie() {
  const cookies = parseCookies(document.cookie)
  return cookies.receipt_csrf
}
