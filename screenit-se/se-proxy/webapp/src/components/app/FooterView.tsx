import React from "react"
import type {EnvironmentInfo} from "../../datatypes/EnvironmentInfo"

export type FooterViewProps = {
	environmentInfo: EnvironmentInfo | null;
};

export default class FooterView extends React.Component<FooterViewProps> {
	render(): JSX.Element {
		return <footer className={"footer"}>
			<div className={"footer-right"}>
				<div
					className={"footer-versie"}>{this.props.environmentInfo && this.props.environmentInfo.version ? `Versie: ${this.props.environmentInfo.version}  Build datum/tijd: ${this.props.environmentInfo.timestamp}` : ""} Applicatie-naam:
					ScreenIT-SE
				</div>
				{this.props.environmentInfo &&
				<div className={"footer-omgeving"}>{`Omgeving: ${this.props.environmentInfo.environment}`}
					{this.props.environmentInfo.nfcEnabled ? "" : ", verplichte NFC authenticatie staat uit."}
				</div>}
			</div>
		</footer>
	}

}