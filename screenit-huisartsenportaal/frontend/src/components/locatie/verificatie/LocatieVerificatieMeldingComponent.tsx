/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */
import styles from "./LocatieVerificatieMeldingComponent.module.scss"
import {useAppSelector} from "../../../index"
import {Col, Row} from "react-bootstrap"
import {useState} from "react"
import {LocatieVerificatieDto} from "../../../state/datatypes/dto/LocatieVerificatieDto"
import LocatieVerificatieModal from "./LocatieVerificatieModal"
import properties from "./LocatieVerificatieMeldingComponent.json"
import {getString} from "../../../util/TekstPropertyUtil"
import classNames from "classnames"

const LocatieVerificatieMeldingComponent = () => {
	const locaties = useAppSelector(state => state.locatieVerificatie)
	const [selectedLocatie, setSelectedLocatie] = useState<LocatieVerificatieDto | undefined>(undefined)

	return (<div className={classNames(styles.style, "px-5", "py-4", "my-2", "rounded")}>
			<LocatieVerificatieModal locatie={selectedLocatie} hide={() => setSelectedLocatie(undefined)}/>
			<p className={"mb-2"}>{getString(properties.title)}</p>
			{locaties.map(locatie => {
				return <Row key={locatie.huisartsportaalId} className={classNames(styles.locatie, "my-2")}>
					<Col sm={4} className={"my-auto"}>
						<span>Locatie</span>
						<span>{locatie.locatieNaam}</span>
					</Col>
					<Col sm={4} className={"my-auto"}>
						<span>Zorgmail klantnummer</span>
						<span>{locatie.zorgmailKlantnummer}</span>
					</Col>
					<Col sm={4} className={styles.col}>
						<button onClick={() => setSelectedLocatie(locatie)} className={"btn btn-primary"}>Locatie verifiëren</button>
					</Col>
				</Row>
			})}
			<p className={"my-2"}>{getString(properties.description[0])}</p>
			<p>{getString(properties.description[1])}</p>
			<p>{getString(properties.description[2])}</p>
		</div>
	)
}

export default LocatieVerificatieMeldingComponent
