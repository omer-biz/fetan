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
    app.ports.saveTheme.subscribe(function (theme: string) {
      localStorage.setItem('theme', theme);
      if (theme === 'dark') {
          document.documentElement.classList.add('dark');
      } else {
          document.documentElement.classList.remove('dark');
      }
    });
}
