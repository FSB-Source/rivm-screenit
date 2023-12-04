import React, {KeyboardEvent} from "react"
import {BrowserRouter} from "react-router-dom"
import TabbarContainer from "./TabbarContainer"
import type {Session} from "../../datatypes/Session"
import {ToastContainer} from "react-toastify"
import "react-toastify/dist/ReactToastify.css"
import {DEFAULT_TOAST_TIMEOUT} from "../../util/ToastUtil"
import WerkstationkiezerContainer from "../login/WerkstationkiezerContainer"
import {Mammograaf} from "../../datatypes/Mammograaf"
import {store} from "../../Store"
import LoginContainer from "../login/LoginContainer"

export type AppProps = {
	session?: Session;
	mammograafId?: number;
	mammografenById: Map<number, Mammograaf>;
};

const AppView = (props: AppProps): JSX.Element => {
	const environmentInfo = store.getState().environmentInfo
	return <div onKeyDown={onKeyPressed}>
		{!props.session ? <div>
			<LoginContainer/>
		</div> : props.mammograafId === undefined && props.mammografenById.size && environmentInfo && (environmentInfo.environment === "Test" || environmentInfo.environment === "PAT") ?
			<WerkstationkiezerContainer/> : <BrowserRouter>
				<div className="app">
					<TabbarContainer/>
				</div>
			</BrowserRouter>}
		<ToastContainer autoClose={DEFAULT_TOAST_TIMEOUT} hideProgressBar position={"top-center"} icon={false}/>
	</div>
}

const onKeyPressed = (e: KeyboardEvent<HTMLDivElement>): boolean => {
	return !(e.ctrlKey && e.code === "KeyJ")
}

export default AppView