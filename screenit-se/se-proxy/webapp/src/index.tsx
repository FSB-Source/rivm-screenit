import React from "react"
import {render} from "react-dom"
import {Provider} from "react-redux"
import {unregister} from "./registerServiceWorker"
import "./index.css"
import "bootstrap/dist/css/bootstrap.min.css"
import "react-bootstrap-table-next/dist/react-bootstrap-table2.min.css"
import "./screenit_se.scss"
import BuitensteView from "./components/app/BuitensteView"
import {store} from "./Store"
import {readEnvironmentInfo} from "./restclient/EnvironmentInfoRestclient"
import {initWebSocket} from "./util/WebSocketUtil"

const root = document.getElementById("root")

if (root) {
	render(<Provider store={store}>
		<BuitensteView/>
	</Provider>, root)
	initWebSocket()
	readEnvironmentInfo()
	unregister()
}