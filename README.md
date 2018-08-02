# Kissvg

Kissvg is a chrome extension for converting web pages into SVG format.

For a sample, checkout the SVG generated for the Hacker News home page.

<a alt="sample.svg" href="sample.svg"><img src="sample.svg" width="274" /></a>

# Installation

Head to the chrome webstore to install the [latest version](https://chrome.google.com/webstore/detail/kissvg/igcglmmgiplpcdhhfdpjkbmfjiplmdkm).

# Development

```
npm install
npm run-script build
```

You can now load the extension from the `chrome/` directory.

# Limitations

* Images are not inlined as data-urls so depending on the viewer some images
  may not render. We may be able to fix this in the future when the
  [chrome.automation](https://developer.chrome.com/extensions/automation) API
  stabilizes.
* If the web page uses web fonts, then you need to install the fonts locally
  for the SVG to render correctly.
* 100% fidelity is tough to acheive and is not our goal, so expect many things
  to break

  
