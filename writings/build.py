#!/usr/bin/env python3
"""
Build script for zapnet writings.
Reads articles.json and generates:
  - /writings/index.html (index page)
  - /writings/<slug>/index.html (per-article pages with OG tags)
All pages load the same compiled elm.js app.
"""

import json
import os
import html

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
ARTICLES_JSON = os.path.join(SCRIPT_DIR, "articles.json")
BASE_URL = "https://zapnet.info"

FONTS = (
    '<link rel="preconnect" href="https://fonts.googleapis.com">'
    '<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>'
    '<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700'
    '&family=Newsreader:ital,wght@0,400;0,500;1,400&display=swap" rel="stylesheet">'
)

BASE_STYLE = "<style>* { margin: 0; padding: 0; } body { background: #08080d; }</style>"


def og_tags(title, description, url, og_type="article", image=""):
    tags = [
        f'<meta property="og:title" content="{html.escape(title)}">',
        f'<meta property="og:description" content="{html.escape(description)}">',
        f'<meta property="og:url" content="{html.escape(url)}">',
        f'<meta property="og:type" content="{og_type}">',
        '<meta property="og:site_name" content="Zapnet">',
    ]
    if image:
        tags.append(f'<meta property="og:image" content="{html.escape(image)}">')
    # Twitter card fallback
    tags.append('<meta name="twitter:card" content="summary">')
    tags.append(f'<meta name="twitter:title" content="{html.escape(title)}">')
    tags.append(f'<meta name="twitter:description" content="{html.escape(description)}">')
    if image:
        tags.append(f'<meta name="twitter:image" content="{html.escape(image)}">')
    return "\n  ".join(tags)


def make_page(title, og_html, elm_js_path, flags_json):
    return f"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{html.escape(title)}</title>
  {og_html}
  {FONTS}
  {BASE_STYLE}
</head>
<body>
  <div id="app"></div>
  <script src="{elm_js_path}"></script>
  <script>
    Elm.Main.init({{
      node: document.getElementById('app'),
      flags: {flags_json}
    }});
  </script>
</body>
</html>
"""


def build():
    with open(ARTICLES_JSON, "r") as f:
        articles = json.load(f)

    # Flags contain all articles (metadata + body)
    flags = {"articles": articles}
    flags_json = json.dumps(flags)

    # Generate index page
    index_og = og_tags(
        title="Writings — Zapnet",
        description="Articles about Zapnet, Nostr, Bitcoin Lightning, and building a value-based internet.",
        url=f"{BASE_URL}/writings/",
        og_type="website",
    )
    index_html = make_page("Writings — Zapnet", index_og, "elm.js", flags_json)
    index_path = os.path.join(SCRIPT_DIR, "index.html")
    with open(index_path, "w") as f:
        f.write(index_html)
    print(f"  wrote {index_path}")

    # Generate per-article pages
    for article in articles:
        slug = article["slug"]
        slug_dir = os.path.join(SCRIPT_DIR, slug)
        os.makedirs(slug_dir, exist_ok=True)

        og_desc = article.get("og_description") or article["description"]
        og_img = article.get("og_image", "")
        article_og = og_tags(
            title=f'{article["title"]} — Zapnet',
            description=og_desc,
            url=f"{BASE_URL}/writings/{slug}/",
            og_type="article",
            image=og_img,
        )
        # elm.js is one level up from the slug directory
        page_html = make_page(
            f'{article["title"]} — Zapnet',
            article_og,
            "../elm.js",
            flags_json,
        )
        page_path = os.path.join(slug_dir, "index.html")
        with open(page_path, "w") as f:
            f.write(page_html)
        print(f"  wrote {page_path}")

    print(f"\nBuilt {len(articles)} article page(s) + index.")


if __name__ == "__main__":
    build()
