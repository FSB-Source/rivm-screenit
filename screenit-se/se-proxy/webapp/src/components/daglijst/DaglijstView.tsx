import React from "react"
import DatumkiezerContainer from "./DatumkiezerContainer"
import AfspraakOverzichtContainer from "./AfspraakOverzichtContainer"
import DagStatistiekenContainer from "../dagverslag/DagStatistiekenContainer"
import PassantAfspraakMakenContainer from "./PassantAfspraakMakenContainer"

const DaglijstView = (): JSX.Element => {
	return <div className="daglijst">
		<div className="daglijst-blokken-rechts">
			<div className="container-fluid">
				<DatumkiezerContainer/>
			</div>
			<DagStatistiekenContainer/>
		</div>
		<PassantAfspraakMakenContainer/>
		<AfspraakOverzichtContainer/>
	</div>
}

export default DaglijstView