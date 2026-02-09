import "./style.css";
import { Elm } from "./Main.elm";

let lessonInfo = localStorage.getItem('lessonInfo');
let flags = lessonInfo ? JSON.parse(lessonInfo) : null;

const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: flags,
});

app.ports.saveInfo.subscribe(function (state: any) {
  localStorage.setItem('lessonInfo', JSON.stringify(state));
});
