
async function getGenericFonts() {
    const genericFamilies = [
        "standard", "sansserif", "serif", "fixed", "cursive", "fantasy"
    ]
    const promises = genericFamilies.map(genericFamily => {
        return new Promise((resolve, reject) => {
            chrome.fontSettings.getFont({genericFamily}, font => {
                if (chrome.runtime.lastError)
                    reject(chrome.runtime.lastError)
                resolve({familyName: genericFamily, fontName: font.fontId})
            })
        })
    })
    
    const fonts = await Promise.all(promises)

    // Add alternate spelling for sansserif
    const sansserif = fonts.find(f => f.familyName == 'sansserif')
    if (sansserif) {
        fonts.push({familyName: 'sans-serif', fontName: sansserif.fontName})
    }
    return fonts
}

async function executeAsyncScript(tabId, file, action, params) {
    return new Promise((resolve, reject) => {

        const actionRunner = async function(extensionId, action, params) {
            const result = {};
            try {
                const actionFn = action.split('.').reduce((o, n) => o[n], window);
                result.value = await actionFn(params)();
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
            const code = `(${actionRunner.toString()})("${extId}", "${action}", ${JSON.stringify(params)})`
            chrome.tabs.executeScript(tabId, {code})
        })
    })
}

async function run(tab) {
    const genericFonts = await getGenericFonts()
    const scriptFile = 'dist/PageScript.js'
    const action = '$kissvg.renderSvg'
    const svg = await executeAsyncScript(tab.id, scriptFile, action, genericFonts)
    const svgDataUrl = "data:image/svg+xml;utf8," + svg    
    chrome.downloads.download({
        url: svgDataUrl,
        filename: 'out.svg',
        conflictAction: 'overwrite'
    })
}

chrome.browserAction.onClicked.addListener(async tab => {
    try {
        chrome.browserAction.disable(tab.id)
        chrome.browserAction.setBadgeText({tabId: tab.id, text: '...'})        
        await run(tab)
    } finally {
        chrome.browserAction.enable(tab.id)
        chrome.browserAction.setBadgeText({tabId: tab.id, text: ''})
    }
})
