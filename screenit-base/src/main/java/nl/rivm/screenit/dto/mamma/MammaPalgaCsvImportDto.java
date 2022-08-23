package nl.rivm.screenit.dto.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;
import java.util.Date;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class MammaPalgaCsvImportDto implements Serializable
{
	private long pseudoId;

	private int geboortejaar;

	private Date aanvangVerrichting;

	private Date eindeVerrichting;

	private Date datumOntvangstMateriaal;

	private Date datumEersteAutorisatie;

	private String verkrijgingswijze;

	private String zijdigheid;

	private String locatie;

	private String locatieInUren;

	private String oestrogeenReceptorStatus;

	private String progesteronReceptorStatus;

	private String her2Status;

	private String bClassificatie;

	private String cClassificatie;

	private String maligniteitsgraad;

	private String pt;

	private String pn;

	private String typeInvasieveTumor;

	private String graderingDcis;

	private String typeNietEenduidigBenigneLaesies;

	private String typeEenduidigBenigneLaesies;

	private String typeCis;

	private String versieProtocol;

	private boolean reedsAangeleverd = false;

	private boolean patid3 = false;

	private int regelNummer;

	private boolean fout = false;

}
