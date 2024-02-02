/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {Col, Row} from "react-bootstrap"
import Select from "react-select"
import LocatieStatusComponent, {LocatieStatusDropdownChoice} from "../../status/LocatieStatusComponent"
import {LocatieStatus} from "../../../../state/datatypes/dto/LocatieDto"
import {useAppThunkDispatch} from "../../../../index"
import {loadingThunkAction} from "../../../../api/LoadingThunkAction"
import {fetchLocaties} from "../../../../api/LocatieThunkAction"
import React from "react"

const locatieStatusOptions: LocatieStatusDropdownChoice[] = [
	{value: LocatieStatus.ACTIEF, label: "actief"},
	{value: LocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD, label: "verifiëren"},
	{value: LocatieStatus.INACTIEF, label: "inactief"},
]

const LocatieTabelFilter = () => {
	const dispatch = useAppThunkDispatch()

	return <Row>
		<Col md={7}/>
		<Col md={5}>
			<Row className={"justify-content-center"}>
				<Col md={7}>
					<p>Uw locaties zijn gefilterd op:</p>
				</Col>
				<Col md={5}>
					<Select<LocatieStatusDropdownChoice>
						defaultValue={locatieStatusOptions[0]}
						formatOptionLabel={(status) => <LocatieStatusComponent status={status.value}/>}
						options={locatieStatusOptions}
						onChange={(value) => {
							value && dispatch(loadingThunkAction(fetchLocaties(value.value)))
						}}
					/>
				</Col>
			</Row>
		</Col>
	</Row>
}

export default LocatieTabelFilter
