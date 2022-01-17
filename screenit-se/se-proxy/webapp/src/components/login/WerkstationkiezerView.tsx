import React, {Component} from "react"
import {Mammograaf} from "../../datatypes/Mammograaf"
import MammograafknopView from "./MammograafknopView"
import WerkstationZonderMammograafknopView from "./WerkstationZonderMammograafknopView"

export type WerkstationkiezerProps = {
	mammografen: Array<Mammograaf>;
};

export default class WerkstationkiezerView extends Component<WerkstationkiezerProps> {

	render(): JSX.Element {
		return <div className={"text-center"} style={{
			marginTop: 130,
		}}>
			<div className={"align-middle"}>Klik op het werkstation waarvan je de werking gaat testen:</div>
			{this.props.mammografen.map((mammograaf: Mammograaf) => <div key={mammograaf.id} style={{
				marginTop: 10,
			}}><MammograafknopView mammograaf={mammograaf}/></div>)}
			<div style={{
				marginTop: 10,
			}}><WerkstationZonderMammograafknopView/></div>
		</div>
	}

}