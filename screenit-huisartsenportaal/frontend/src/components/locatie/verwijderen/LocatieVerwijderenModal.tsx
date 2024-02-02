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
import {Button, Col, Modal, Row} from "react-bootstrap"
import {getString} from "../../../util/TekstPropertyUtil"
import properties from "./LocatieVerwijderenModal.json"
import {LocatieDto} from "../../../state/datatypes/dto/LocatieDto"
import React from "react"

export interface LocatieVerwijderenModalProps {
	locatie?: LocatieDto;
	onHide: () => void;
	onVerwijder: () => void;
}

const LocatieVerwijderenModal = (props: LocatieVerwijderenModalProps) => {
	return <Modal size={"lg"} show={!!props.locatie} onHide={props.onHide}>
		<Modal.Header closeButton>
			<Modal.Title>{getString(properties.title)}</Modal.Title>
		</Modal.Header>
		<Modal.Body>
			<Row>
				<Col>{getString(properties.body)}</Col>
			</Row>
		</Modal.Body>
		<Modal.Footer>
			<Button variant="secondary" onClick={props.onHide}>
				{getString(properties.button.close)}
			</Button>
			<Button variant="primary" onClick={props.onVerwijder}>
				{getString(properties.button.submit)}
			</Button>
		</Modal.Footer>
	</Modal>
}

export default LocatieVerwijderenModal
