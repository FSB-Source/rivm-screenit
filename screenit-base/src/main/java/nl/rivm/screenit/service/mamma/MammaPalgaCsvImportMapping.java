package nl.rivm.screenit.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class MammaPalgaCsvImportMapping
{

	private int pseudoId = -1;

	private int geboortejaar = -1;

	private int aanvangVerrichting = -1;

	private int eindeVerrichting = -1;

	private int datumOntvangstMateriaal = -1;

	private int datumEersteAutorisatie = -1;

	private int verkrijgingswijze = -1;

	private int zijdigheid = -1;

	private int locatie = -1;

	private int locatieInUren = -1;

	private int oestrogeenReceptorStatus = -1;

	private int progesteronReceptorStatus = -1;

	private int her2Status = -1;

	private int bClassificatie = -1;

	private int cClassificatie = -1;

	private int maligniteitsgraad = -1;

	private int pt = -1;

	private int pn = -1;

	private int typeInvasieveTumor = -1;

	private int graderingDcis = -1;

	private int typeNietEenduidigBenigneLaesies = -1;

	private int typeEenduidigBenigneLaesies = -1;

	private int typeCis = -1;

	private int versieProtocol = -1;

	private int isReedsAangeleverd = -1;

	private int matchniveau = -1;

}
