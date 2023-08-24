/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import React, {useState} from "react"
import styles from "./LocatieTabel.module.scss"
import properties from "./LocatieTabel.json"
import {getString} from "../../../util/TekstPropertyUtil"
import {useAppSelector, useAppThunkDispatch} from "../../../index"
import {LocatieDto, LocatieStatus} from "../../../state/datatypes/dto/LocatieDto"
import LocatieStatusComponent from "../status/LocatieStatusComponent"
import LocatieVerwijderenModal from "../verwijderen/LocatieVerwijderenModal"
import {loadingThunkAction} from "../../../api/LoadingThunkAction"
import {putLocatie} from "../../../api/LocatieThunkAction"
import LocatieToevoegenModal from "../toevoegen/LocatieToevoegenModal"
import {AdresDto} from "../../../state/datatypes/dto/AdresDto"
import LocatieTabelFilter from "./filter/LocatieTabelFilter"

export interface LocatieTabelProps {
	nawGegevens?: AdresDto;
}

const LocatieTabel = (props: LocatieTabelProps) => {
	const dispatch = useAppThunkDispatch()
	const auth = useAppSelector((state => state.oauth))

	const [wijzigLocatie, setWijzigLocatie] = useState<LocatieDto | undefined>(undefined)
	const [verwijderLocatie, setVerwijderLocatie] = useState<LocatieDto | undefined>(undefined)
	const state = useAppSelector((state => state.locaties))

	return (
		<div className={styles.style}>
			<LocatieTabelFilter/>
			<table>
				<LocatieToevoegenModal show={!!wijzigLocatie} onHide={() => setWijzigLocatie(undefined)} initialValues={wijzigLocatie} nawGegevens={props.nawGegevens}/>
				<LocatieVerwijderenModal locatie={verwijderLocatie} onHide={() => setVerwijderLocatie(undefined)} onVerwijder={() => {
					verwijderLocatie && dispatch(loadingThunkAction(putLocatie({
						...verwijderLocatie,
						status: LocatieStatus.INACTIEF,
					}))).then(() => setVerwijderLocatie(undefined))
				}}/>
				<thead>
				<tr>
					<th>{getString(properties.table.headers.naam)}</th>
					<th>{getString(properties.table.headers.adres)}</th>
					<th>{getString(properties.table.headers.postcode)}</th>
					<th>{getString(properties.table.headers.plaats)}</th>
					<th>{getString(properties.table.headers.status)}</th>
					{auth?.scope !== "registreren" && <th>{getString(properties.table.headers.wijzigen)}</th>}
					<th>{getString(properties.table.headers.verwijderen)}</th>
				</tr>
				</thead>
				<tbody>
				{state.values.locaties.map(locatie => {
					return <tr key={locatie.huisartsId}>
						<td>{locatie.naam}</td>
						<td>{locatie.locatieAdres.straat} {locatie.locatieAdres.huisnummer} {locatie.locatieAdres.huisnummertoevoeging}</td>
						<td>{locatie.locatieAdres.postcode}</td>
						<td>{locatie.locatieAdres.woonplaats.naam}</td>
						<td><LocatieStatusComponent status={locatie.status}/></td>
						<td>{(auth?.scope !== "registreren" && locatie.status !== LocatieStatus.INACTIEF) &&
							<i className="bi bi-pencil" onClick={() => setWijzigLocatie(locatie)}/>}</td>
						<td>{(locatie.status !== LocatieStatus.INACTIEF) &&
							<i className="bi bi-trash3" onClick={() => setVerwijderLocatie(locatie)}/>}</td>
					</tr>
				})}
				</tbody>
			</table>
		</div>
	)
}

export default LocatieTabel
