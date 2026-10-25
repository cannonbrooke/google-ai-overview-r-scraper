# PUBLIC EXAMPLE: Collect Google AI Overview text with RStudio + Chrome (macOS)
# No questions, names, answers, paths, or research files from the original project.
# This is a reusable example, not a guarantee against changes to Google's HTML.
# Google may limit automated requests. Do not bypass challenges or CAPTCHA.
#
# 1. Create a CSV named questions.csv in your RStudio working directory:
#      id,question
#      Q001,What is a library?
#      Q002,What is an archive?
#    (Use your own questions; keep identifiers unique.)
# 2. Chrome: View > Developer > Allow JavaScript from Apple Events.
#    Give RStudio permission to control Chrome if macOS requests it.
# 3. Install required package once: install.packages("jsonlite")
# 4. Run: source("chrome_ai_overview_public.R")
# 5. Output: google_ai_overview_responses.csv
#    Each row has question, status, captured text, URLs, date and any warning.
#    Review text manually: Google's citation cards can contaminate extraction.

if (Sys.info()[["sysname"]] != "Darwin") stop("This example requires macOS.")
if (!requireNamespace("jsonlite", quietly=TRUE)) stop('Install jsonlite: install.packages("jsonlite")')

input_file <- "questions.csv"
output_file <- "google_ai_overview_responses.csv"
wait_seconds <- 30L
min_pause <- 20
max_pause <- 45
max_queries <- Inf  # Change to 2 for a quick test.

if (!file.exists(input_file)) stop("Create questions.csv first (columns: id, question).")
q <- read.csv(input_file, stringsAsFactors=FALSE, check.names=FALSE, fileEncoding="UTF-8-BOM")
if (!all(c("id","question") %in% names(q))) stop("CSV must have 'id' and 'question' columns.")
q <- q[, c("id","question")]
q$id <- trimws(as.character(q$id)); q$question <- trimws(as.character(q$question))
if (!nrow(q) || any(!nzchar(q$id)) || any(!nzchar(q$question)) || anyDuplicated(q$id))
  stop("Questions must have unique nonempty ids and nonempty question text.")
q <- head(q, max_queries)

old <- if (file.exists(output_file)) read.csv(output_file, stringsAsFactors=FALSE,
  check.names=FALSE, fileEncoding="UTF-8-BOM") else NULL
if (!is.null(old)) {
  if (!all(c("id","question","status") %in% names(old))) stop("Existing output has unexpected columns. Rename it before rerunning.")
  hits <- match(old$id, q$id)
  if (any(is.na(hits)) || any(old$question != q$question[hits]))
    stop("Existing output does not match questions.csv. Rename the existing file before a new collection.")
}

# AppleScript is stored temporarily; questions never become AppleScript source.
apple_run <- function(lines) {
  f <- tempfile(fileext=".applescript")
  on.exit(unlink(f), add=TRUE)
  writeLines(lines,f,useBytes=TRUE)
  result <- suppressWarnings(system2("osascript", args=shQuote(f), stdout=TRUE,stderr=TRUE))
  exitcode <- attr(result,"status")
  if (!is.null(exitcode) && exitcode != 0L) stop(paste(result,collapse="\n"))
  paste(result,collapse="\n")
}
as_str <- function(x) encodeString(x, quote='"')
window_id <- suppressWarnings(as.integer(apple_run(c(
  'tell application "Google Chrome"', 'activate',
  'set w to make new window',
  'set URL of active tab of w to "about:blank"',
  'return id of w', 'end tell'))))
if (is.na(window_id)) stop("Could not open Chrome window.")
apple_js <- function(js) {
  js <- gsub("(?m)^\\s*//[^\\r\\n]*", "", js, perl=TRUE)
  js <- gsub("[\\r\\n]+", " ", js, perl=TRUE)
  apple_run(c('tell application "Google Chrome"',
    sprintf('set w to first window whose id is %s',window_id),
    paste0('return execute active tab of w javascript ',as_str(js)), 'end tell'))
}
test <- tryCatch(apple_js('JSON.stringify({ready:true})'), error=function(e) {
  stop("Enable Chrome > View > Developer > Allow JavaScript from Apple Events, and allow RStudio to control Chrome.\n",conditionMessage(e))
})
if (!grepl('"ready":true',test,fixed=TRUE)) stop("Chrome JavaScript not enabled.")
navigate <- function(url) apple_run(c('tell application "Google Chrome"',
  sprintf('set w to first window whose id is %s',window_id),
  paste0('set URL of active tab of w to ',as_str(url)),'end tell'))

probe_js <- r"----((() => {
  const clean = s => (s || '').replace(/\u00a0/g, ' ').replace(/[ \t]+/g, ' ').trim();
  const t = el => clean(el && (el.innerText || el.textContent) || '');
  const body = t(document.body), url = location.href;
  const lower = body.toLowerCase();
  const blocked = /our systems have detected unusual traffic|verify you are a human|not a robot|recaptcha|unusual traffic from your computer network|sorry.*automated queries/.test(lower) || /\/sorry\//.test(url);
  const consent = (/before you continue to google search|choose what you're giving consent to|accept all|reject all/.test(lower) && !document.querySelector('#search'));
  const fail = (status, reason) => ({status, text:'', raw_text:'', urls:[], reason, title:document.title,
    selector:'', html:'', cutoff:-1, source_cards:[], bounds:null});
  if (blocked || consent) return fail('Blocked', blocked ? 'Google challenge / unusual traffic' : 'Consent page');
  const visible = el => !!(el && el.getClientRects().length);
  const selectors = ['[data-attrid="ai_overview"]','[data-attrid*="ai_overview"]',
    '[data-attrid*="overview"]','[aria-label="AI Overview"]','[aria-label="AI overview"]'];
  const headings = [...document.querySelectorAll('[role="heading"],h1,h2,h3,h4,strong')]
    .filter(el => visible(el) && /^ai overview(s)?$/i.test(t(el)));
  const candidates = [];
  const seen = new Set();
  const add = (node, why) => {
    if (!node || !visible(node) || seen.has(node)) return;
    const raw=t(node);
    if (raw.length < 90 || raw.length > 12000 || !/^AI Overview/i.test(raw)) return;
    seen.add(node); candidates.push({node,raw,why});
  };
  for (const h of headings) {
    let e=h;
    for (let j=0;e && j<10;j++,e=e.parentElement) add(e,'heading ancestor');
  }
  for (const sel of selectors)
    for (const e of [...document.querySelectorAll(sel)].slice(0,25)) add(e,sel);
  candidates.sort((a,b)=>a.raw.length-b.raw.length);
  if (candidates.length === 0) {
    if (headings.length) return fail('Review','AI Overview heading visible, but no matching answer container');
    if (document.querySelector('#search') && body.length > 120) return fail('No','Search results visible without AI Overview heading');
    return fail('Review','Google page not ready or search results not reliably identified');
  }
  const item=candidates[0], node=item.node, raw=item.raw;
  node.setAttribute('data-public-aio-capture','true');
  const anchors=[...node.querySelectorAll('a[href]')].filter(a=>visible(a))
    .map(a=>({a, href:a.href, label:t(a)}))
    .filter(x=>/^https?:\/\//.test(x.href) && (
      !/^(https?:\/\/)?(www\.)?google\.[a-z.]+\//.test(x.href) ||
      /google\.[a-z.]+\/url\?/.test(x.href)));
  const urls=[...new Set(anchors.map(x=>x.href))].slice(0,40);
  // Google's source cards may not contain h3 links. Identify linked *blocks*
  // by their text and nearby card containers rather than treating every link
  // as a standalone citation. Their text usually follows the answer.
  // Source card detection uses two independent signals:
  // (1) a long linked result card, or (2) a search-result title followed by
  // a published-date/snippet line. The latter covers Google's compact cards.
  const sourceCards=[];
  const citationLabels=new Set();
  const lineClean = s => clean(s).replace(/\s+/g,' ').trim();
  for (const el of node.querySelectorAll('a[href],button,[role="button"]')) {
    const label=t(el);
    if (!label) continue;
    for (const line of label.split(/\n+/)) {
      const v=lineClean(line);
      if (v.length>1 && v.length<=65) citationLabels.add(v);
    }
  }
  for (const {a,href,label} of anchors) {
    const titleEl = a.querySelector('h3,[role="heading"]');
    if (!titleEl && label.length<40) continue;
    let card=a;
    for (let j=0;j<5 && card.parentElement && card.parentElement!==node;j++) {
      const p=card.parentElement;
      const x=t(p);
      if (x.length>900 || /^AI Overview/i.test(x)) break;
      // keep climbing if the ancestor includes publication metadata or a snippet
      if ((/\b(?:19|20)\d{2}\s*[—–-]/.test(x) || /\b(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s+\d{1,2},?\s+(?:19|20)\d{2}/.test(x)) && x.length<850) {
        card=p; break;
      }
      if (x.length <= 500) card=p;
    }
    const ct=t(card);
    const looksCard= !!titleEl ||
      ((label.length>=30 || ct.length>=75) &&
       (/\b(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s+\d{1,2},?\s+(?:19|20)\d{2}/.test(ct) ||
        /\b(?:19|20)\d{2}\s*[—–-]/.test(ct)));
    if (looksCard) {
      const first=lineClean(ct).slice(0,90);
      // Only anchor a boundary on the article TITLE; a publisher name might
      // also appear earlier as a valid inline citation.
      const title = titleEl ? lineClean(t(titleEl)) :
        label.split('\n').map(lineClean).find(z => z.length>=32) || '';
      const at=title ? raw.indexOf(title,120) : -1;
      if (at>120) sourceCards.push({position:at, first, link:href, preview:ct.slice(0,180)});
    }
  }
  const lines=raw.split('\n');
  const offsets=[];
  for(let i=0,pos=0;i<lines.length;i++) {offsets.push(pos);pos+=lines[i].length+1;}
  const dateSnippet = /^(?:[A-Z][a-z]{2,9}\s+\d{1,2},?\s+(?:19|20)\d{2}|\d{1,2}\s+[A-Z][a-z]{2,9}\s+(?:19|20)\d{2}|\d+[smhd])\s*[—–-]/;
  let inferredCard=-1;
  for(let i=2;i<lines.length;i++) {
    const s=lineClean(lines[i]);
    if (!dateSnippet.test(s)) continue;
    // Require an article title directly before the date/snippet line.
    let j=i-1;
    while(j>=0 && !lineClean(lines[j])) j--;
    const title=j>=0 ? lineClean(lines[j]) : '';
    if (title.length<18 || (!title.includes('...') && !/[.:!?]/.test(title))) continue;
    let first=j;
    // Include preceding source-name labels (e.g., WBAL-TV / WBAL-TV).
    while(first>0) {
      const previous=lineClean(lines[first-1]);
      if (!previous || previous.length>50 || /[.!?;:]$/.test(previous)) break;
      if (!citationLabels.has(previous) && !/^[+]?\d+$/.test(previous)) break;
      first--;
    }
    if(offsets[first] > 120) {inferredCard=offsets[first]; break;}
  }
  const boundaries=sourceCards.map(c=>c.position).filter(p=>p>=120);
  if(inferredCard>=120) boundaries.push(inferredCard);
  const cutoff=boundaries.length ? Math.min(...boundaries) : -1;
  const answerRegion=(cutoff>=120 ? raw.slice(0,cutoff) : raw).replace(/^AI Overview(?:s)?\s*/i,'');
  // Strip ONLY standalone short citation labels, not arbitrary mentions of a
  // source in AI-generated prose. Preserve headings like "Key Points".
  let answer=answerRegion.split('\n').map(line=>lineClean(line))
    .filter(line=>line && !/^\+\d+$/.test(line) &&
       !(citationLabels.has(line) && !/[.!?;:]$/.test(line)))
    .join('\n').replace(/\n(?:Show all|Feedback)\s*$/i,'').trim();
  // A source-publisher label may be rendered as plain text rather than as an
  // anchor (e.g. an isolated "Quora" after a complete final paragraph).
  // Remove it ONLY at the very end of the answer, and only if it matches an
  // observed source hostname, a citation label, or a narrowly known site name.
  const siteKey=s=>lineClean(s).toLowerCase().replace(/[^a-z0-9]/g,'');
  const siteNames = new Set(['quora','reddit','youtube','wbaltv']);
  for (const u of urls) {
    try {
      const domain=new URL(u).hostname.replace(/^(www|m)\./,'').split('.')[0];
      if(domain.length>2) siteNames.add(siteKey(domain));
    } catch(e) {}
  }
  for (const label of citationLabels) {
    if (label.length>=3 && label.length<=45) siteNames.add(siteKey(label));
  }
  const answerLines = answer.split('\n');
  const removedTrailingLabels=[];
  while (answerLines.length>=2) {
    const last=lineClean(answerLines[answerLines.length-1]);
    const prev=lineClean(answerLines[answerLines.length-2]);
    if (last.length>0 && last.length<=45 && !/[.!?;:]$/.test(last) &&
        /[.!?]$/.test(prev) && siteNames.has(siteKey(last))) {
      removedTrailingLabels.unshift(last);
      answerLines.pop();
    } else break;
  }
  answer=answerLines.join('\n').trim();
  let status='Yes', reason='';
  if (answer.length<65) {status='Review'; reason='Generated answer too short to trust';}
  if (cutoff<0 && /\b(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s+\d{1,2},?\s+(?:19|20)\d{2}\s*[—–-]/.test(answer)) {
    status='Review'; reason='Possible source card with publication date remains';
  }
  if (cutoff<0 && raw.length>700 && anchors.some(x=>x.label.length>=40)) {
    status='Review'; reason='Long overview has linked blocks but no safe card boundary';
  }
  const r=node.getBoundingClientRect();
  const bounds={left:r.left,top:r.top,width:r.width,height:r.height,
    innerWidth:window.innerWidth,innerHeight:window.innerHeight,
    outerWidth:window.outerWidth,outerHeight:window.outerHeight};
  return {status,text:answer,raw_text:raw,urls,reason,title:document.title,
    selector:item.why,html:node.outerHTML,cutoff,source_cards:sourceCards.slice(0,20),citation_labels:[...citationLabels].slice(0,80),removed_trailing_labels:removedTrailingLabels,bounds};
})())----"
expand_js <- r"----((() => {
  const nodes = [...document.querySelectorAll('button,[role="button"],div[aria-label]')];
  const root = document.querySelector('[data-public-aio-capture="true"]');
  const btn = nodes.find(el => /show more ai overview|expand ai overview/i.test(
    el.getAttribute('aria-label') || '')) ||
    (root ? [...root.querySelectorAll('button,[role="button"]')].find(el =>
      /^show more$/i.test((el.innerText || '').trim())) : null);
  if (btn) { btn.click(); return true; }
  return false;
})())----"


get_probe <- function() {
  out <- apple_js(paste0('JSON.stringify(',probe_js,')'))
  if (!nzchar(out) || identical(out,"missing value")) stop("Empty browser response.")
  jsonlite::fromJSON(out,simplifyVector=TRUE)
}

results <- if (is.null(old)) data.frame(id=character(),question=character(),status=character(),
  overview_text=character(),source_urls=character(),captured_at=character(),note=character(),
  stringsAsFactors=FALSE) else old

for (i in seq_len(nrow(q))) {
  id <- q$id[i]; question <- q$question[i]
  if (id %in% results$id) { cat("Already captured:",id,"\n"); next }
  cat(sprintf("[%d/%d] %s\n", i,nrow(q),id))
  url <- paste0("https://www.google.com/search?q=",utils::URLencode(question,reserved=TRUE),
                "&hl=en&gl=us&pws=0&num=10")
  value <- tryCatch({
    navigate(url)
    Sys.sleep(stats::runif(1,5,9))
    response <- NULL
    for (j in seq_len(max(1L,ceiling(wait_seconds/5)))) {
      response <- get_probe()
      if (response$status %in% c("Yes","Blocked")) break
      if (j < ceiling(wait_seconds/5)) Sys.sleep(5)
    }
    if (identical(response$status,"Yes")) {
      clicked <- tryCatch(apple_js(paste0('String(',expand_js,')')),error=function(e) "false")
      if (identical(clicked,"true")) {Sys.sleep(2); response <- get_probe()}
    }
    response
  },error=function(e) list(status="Review",text="",urls=character(),reason=conditionMessage(e)))
  status <- if (is.null(value$status)) "Review" else as.character(value$status)
  txt <- if (is.null(value$text)) "" else as.character(value$text)
  urls <- if (is.null(value$urls)) character() else as.character(value$urls)
  note <- if (is.null(value$reason)) "" else as.character(value$reason)
  results <- rbind(results, data.frame(id=id,question=question,status=status,
    overview_text=txt,source_urls=paste(urls,collapse=" | "),
    captured_at=format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z"),note=note,
    stringsAsFactors=FALSE))
  write.csv(results, output_file,row.names=FALSE,na="",fileEncoding="UTF-8")
  cat("  ",status," / ",nchar(txt)," characters\n",sep="")
  if (status == "Blocked") {
    message("Google challenge detected. Stopped; no CAPTCHA automation or bypass.")
    break
  }
  if (i < nrow(q)) Sys.sleep(stats::runif(1,min_pause,max_pause))
}
cat("Results saved: ", normalizePath(output_file,mustWork=TRUE),"\n",sep="")