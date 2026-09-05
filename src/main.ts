import "./style.css";
import { Elm } from "./Main.elm";

let lessonInfo = localStorage.getItem('lessonInfo');
let flagsInfo = lessonInfo ? JSON.parse(lessonInfo) : null;

const savedTheme = localStorage.getItem('theme') || 'dark';
if (savedTheme === 'dark') {
    document.documentElement.classList.add('dark');
} else {
    document.documentElement.classList.remove('dark');
}

const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: {
      lessonInfo: flagsInfo,
      theme: savedTheme
  },
});

app.ports.saveInfo.subscribe(function (state: any) {
  localStorage.setItem('lessonInfo', JSON.stringify(state));
});

if (app.ports.saveTheme) {
    app.ports.saveTheme.subscribe(async function (theme: string) {
        localStorage.setItem('theme', theme);
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
                        clipPath: clipPath,
                    },
                    {
                        duration: 500,
                        easing: "ease-in-out",
                        // Always animate the NEW snapshot expanding to avoid reverse/z-index issues
                        pseudoElement: "::view-transition-new(root)",
                    }
                );
            });
        });
    });
}
