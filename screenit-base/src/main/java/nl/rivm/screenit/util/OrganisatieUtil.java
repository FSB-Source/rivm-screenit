package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Collectors;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Mammapoli;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.RadiologieAfdeling;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ZorgInstelling;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.PaLaboratorium;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class OrganisatieUtil
{

	public static String getOrganisatieNamen(List<Instelling> instellingen)
	{
		return instellingen.stream().map(Instelling::getNaam).collect(Collectors.joining(", "));
	}

	public static Instelling maakOrganisatie(OrganisatieType organisatieType, Instelling binnenOrganisatie)
	{
		Instelling nieuweOrganisatie;
		var organisatieTypeGerelateerdeOrganisatie = binnenOrganisatie != null ? binnenOrganisatie.getOrganisatieType() : null;
		switch (organisatieType)
		{
		case RIVM:
			nieuweOrganisatie = new Rivm();
			break;
		case MAMMAPOLI:
			nieuweOrganisatie = new Mammapoli();
			break;
		case RADIOLOGIEAFDELING:
			nieuweOrganisatie = new RadiologieAfdeling();
			break;
		case INTAKELOCATIE:
			nieuweOrganisatie = new ColonIntakelocatie();
			if (organisatieTypeGerelateerdeOrganisatie == OrganisatieType.ZORGINSTELLING)
			{
				nieuweOrganisatie.setParent(binnenOrganisatie);
			}
			break;
		case SCREENINGSORGANISATIE:
			nieuweOrganisatie = new ScreeningOrganisatie();
			((ScreeningOrganisatie) nieuweOrganisatie).setAfspraakDrempelBk(10);
			((ScreeningOrganisatie) nieuweOrganisatie).setFactorMinderValideBk(new BigDecimal("3.0"));
			((ScreeningOrganisatie) nieuweOrganisatie).setFactorDubbeleTijdBk(new BigDecimal("2.0"));
			((ScreeningOrganisatie) nieuweOrganisatie).setFactorEersteOnderzoekBk(new BigDecimal("1.1"));
			break;
		case LABORATORIUM:
			nieuweOrganisatie = new IFobtLaboratorium();
			break;
		case PA_LABORATORIUM:
			nieuweOrganisatie = new PaLaboratorium();
			break;
		case BMHK_LABORATORIUM:
			nieuweOrganisatie = new BMHKLaboratorium();
			break;
		case HUISARTS:
			nieuweOrganisatie = new CervixHuisarts();
			break;
		case ZORGINSTELLING:
			nieuweOrganisatie = new ZorgInstelling();
			if (organisatieTypeGerelateerdeOrganisatie == OrganisatieType.SCREENINGSORGANISATIE)
			{
				nieuweOrganisatie.setParent(binnenOrganisatie);
			}
			break;
		case COLOSCOPIELOCATIE:
			nieuweOrganisatie = new ColoscopieLocatie();
			if (organisatieTypeGerelateerdeOrganisatie == OrganisatieType.ZORGINSTELLING)
			{
				nieuweOrganisatie.setParent(binnenOrganisatie);
			}
			break;
		case CENTRALE_EENHEID:
			nieuweOrganisatie = new CentraleEenheid();
			if (organisatieTypeGerelateerdeOrganisatie == OrganisatieType.SCREENINGSORGANISATIE)
			{
				nieuweOrganisatie.setRegio(binnenOrganisatie);
			}
			break;
		case BEOORDELINGSEENHEID:
			nieuweOrganisatie = new BeoordelingsEenheid();
			break;
		default:
			nieuweOrganisatie = new Instelling();
			break;
		}
		nieuweOrganisatie.setOrganisatieType(organisatieType);
		nieuweOrganisatie.setActief(true);
		return nieuweOrganisatie;
	}
}
