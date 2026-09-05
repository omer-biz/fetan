import "./style.css";
import { Elm } from "./Main.elm";

import { get, set } from 'idb-keyval';

(async function initApp() {
    // Migrate legacy localStorage data to IndexedDB
    const legacyLesson = localStorage.getItem('lessonInfo');
    if (legacyLesson) {
        const parsed = JSON.parse(legacyLesson);
        await set('lessonInfo', parsed);
        localStorage.removeItem('lessonInfo');
    }
    
    const legacyTheme = localStorage.getItem('theme');
    if (legacyTheme) {
        await set('theme', legacyTheme);
        localStorage.removeItem('theme');
    }

    // Load from IndexedDB
    let flagsInfo = await get('lessonInfo') || null;
    let savedTheme = await get('theme') || 'dark';

    if (savedTheme === 'dark') {
        document.documentElement.classList.add('dark');
    } else {
        document.documentElement.classList.remove('dark');
    }

    const app = Elm.Main.init({
      flags: {
          lessonInfo: flagsInfo,
          theme: savedTheme,
          now: Date.now()
      },
    });

    app.ports.saveInfo.subscribe(async function (state: any) {
      await set('lessonInfo', state);
    });

    if (app.ports.saveTheme) {
        app.ports.saveTheme.subscribe(async function (theme: string) {
            await set('theme', theme);
        const isDark = theme === 'dark';

        if (!document.startViewTransition) {
            document.documentElement.classList.toggle('dark', isDark);
            return;
        }

        const button = document.getElementById('theme-toggle');
        // Account for scroll position
        const x = button ? button.getBoundingClientRect().left + window.scrollX + button.getBoundingClientRect().width / 2 : window.innerWidth / 2;
        const y = button ? button.getBoundingClientRect().top + window.scrollY + button.getBoundingClientRect().height / 2 : window.innerHeight / 2;
        
        // Calculate radius to cover the entire scrollable document, not just viewport
        const docWidth = Math.max(document.documentElement.scrollWidth, window.innerWidth);
        const docHeight = Math.max(document.documentElement.scrollHeight, window.innerHeight);
        const endRadius = Math.hypot(Math.max(x, docWidth - x), Math.max(y, docHeight - y));

        // Wait for Elm to render the DOM changes before taking the snapshot
        requestAnimationFrame(() => {
            const transition = document.startViewTransition(() => {
                document.documentElement.classList.toggle('dark', isDark);
            });

            transition.ready.then(() => {
                const clipPath = [
                    `circle(0px at ${x}px ${y}px)`,
                    `circle(${endRadius}px at ${x}px ${y}px)`
                ];
                
                document.documentElement.animate(
                    {
                        clipPath: isDark ? clipPath : [...clipPath].reverse(),
                    },
                    {
                        duration: 500,
                        easing: "ease-in-out",
                        pseudoElement: isDark ? "::view-transition-new(root)" : "::view-transition-old(root)",
                        fill: 'forwards',
                    }
                );
            });
        });
    });
}

})();

// Smooth animated caret
let caretElement: HTMLElement | null = null;
let caretBorder: HTMLElement | null = null;

function updateCaret() {
    const active = document.getElementById('active-letter');
    if (active) {
        if (!caretElement) {
            caretElement = document.createElement('div');
            caretElement.className = 'absolute top-0 left-0 bg-slate-500/10 dark:bg-slate-400/20 rounded-sm pointer-events-none z-0';
            caretElement.style.transition = 'transform 0.15s ease-out, width 0.15s ease-out, height 0.15s ease-out';
            caretElement.style.transformOrigin = 'top left';
            
            caretBorder = document.createElement('div');
            caretBorder.className = 'absolute -left-[1px] top-[10%] h-[80%] w-[3px] bg-slate-500 dark:bg-slate-400 animate-blink rounded-full shadow-[0_0_3px_rgba(100,116,139,0.5)]';
            
            caretElement.appendChild(caretBorder);
            document.body.appendChild(caretElement);
        }
        
        const rect = active.getBoundingClientRect();
        const top = rect.top + window.scrollY;
        const left = rect.left + window.scrollX;
        
        caretElement.style.transform = `translate(${left}px, ${top}px)`;
        caretElement.style.width = `${rect.width}px`;
        caretElement.style.height = `${rect.height}px`;
        caretElement.style.opacity = '1';
    } else if (caretElement) {
        caretElement.style.opacity = '0';
    }
    requestAnimationFrame(updateCaret);
}
requestAnimationFrame(updateCaret);
