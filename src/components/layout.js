import { Link } from "gatsby";
import { ThemeToggler } from "gatsby-plugin-dark-mode";
import React from "react";
import moon from "../assets/moon.png";
import sun from "../assets/sun.png";
import { rhythm } from "../utils/typography";
//import Excalidraw from "./excalidraw";
import "./layoutStyles.css";
import Toggle from "./Toggle";

const Layout = ({ location, title, children, parentClassName }) => {
  const rootPath = `${__PATH_PREFIX__}/`;
  return (
    <div
      style={{
        marginLeft: `auto`,
        marginRight: `auto`,
        maxWidth: rhythm(26),
        padding: `${rhythm(1.5)} ${rhythm(3 / 4)}`,
      }}
    >
      <div
        style={{
          position: "fixed",
          right: "8px",
          top: "8px",
        }}
      >
        <ThemeToggler>
          {({ theme, toggleTheme }) => {
            return (
              <Toggle
                icons={{
                  checked: (
                    <img
                      src={moon}
                      width="16"
                      height="16"
                      alt="presentation"
                      style={{ pointerEvents: "none" }}
                    />
                  ),
                  unchecked: (
                    <img
                      src={sun}
                      width="16"
                      height="16"
                      alt="presentation"
                      style={{ pointerEvents: "none" }}
                    />
                  ),
                }}
                checked={theme === "dark"}
                onChange={() => {
                  toggleTheme(theme === "light" ? "dark" : "light");
                }}
              />
            );
          }}
        </ThemeToggler>
      </div>
      <header>
        <div style={{ fontFamily: "var(--ui-font)", textAlign: "left" }}>
          <span>
            <Link to="/">All posts</Link>
          </span>
          &nbsp;&nbsp;
          <span>
            <Link to="/research">Research</Link>
          </span>
        </div>
        <span style={{ clear: "both" }} />
        {location.pathname === rootPath ? <h1>{title}</h1> : null}
      </header>
      <main className={parentClassName}>{children}</main>
      <footer
        style={{
          textAlign: "center",
          padding: `${rhythm(2)} 0`,
        }}
      >
        <span>
          © {new Date().getFullYear()} SpinoWeb
          {" • "}
          <a href="https://twitter.com/spinoweb">Twitter</a>
          {" • "}
          <a href="https://github.com/spinoweb/exblog">Source Code</a>
          {" • "}
          <a href="https://github.com/spinoweb/exblog/blob/master/LICENSE">
            MIT Licensed
          </a>
        </span>
      </footer>
    </div>
  );
};

export default Layout;
