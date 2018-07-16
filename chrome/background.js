
async function getGenericFonts() {
    const genericFamilies = [
        "standard", "sansserif", "serif", "fixed", "cursive", "fantasy"
    ]
    const promises = genericFamilies.map(genericFamily => {
        return new Promise((resolve, reject) => {
            chrome.fontSettings.getFont({genericFamily}, font => {
                if (chrome.runtime.lastError)
                    reject(chrome.runtime.lastError)
                resolve({genericFamily, font: font.fontId})
            })
        })
    })
    return await Promise.all(promises)
}

async function executeAsyncScript(tabId, file, action) {
    return new Promise((resolve, reject) => {

        const actionRunner = async function(extensionId, action) {
            const result = {};
            try {
                const actionFn = action.split('.').reduce((o, n) => o[n], window);
                result.value = await actionFn();
            } catch(e) {
                result.error = e.message;
            } finally {
                chrome.runtime.sendMessage(extensionId, result);
            }
        }
        
        const responseListener = result => {
            chrome.runtime.onMessage.removeListener(responseListener)
            if (chrome.runtime.lastError || result.error) {
                reject(chrome.runtime.lastError || result.error)
            } else {
                resolve(result.value)
            }
        }
        
        chrome.tabs.executeScript(tabId, {file}, () => {
            chrome.runtime.onMessage.addListener(responseListener)
            const extId = chrome.runtime.id
            const code = `(${actionRunner.toString()})("${extId}", "${action}")`
            chrome.tabs.executeScript(tabId, {code})
        })
    })
}

chrome.browserAction.onClicked.addListener(async tab => {
    const genericFonts = await getGenericFonts()
    const scriptFile = 'dist/PageScript.js'
    const action = '$kissvg.renderSvg'
    const svg = await executeAsyncScript(tab.id, scriptFile, action)
    const svgDataUrl = "data:image/svg+xml;utf8," + svg    
    chrome.downloads.download({
        url: svgDataUrl,
        filename: 'out.svg',
        conflictAction: 'overwrite'
    })
})
