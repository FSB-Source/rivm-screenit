package nl.rivm.screenit.mamma.se.dto.onderzoek;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.util.List;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.mamma.se.dto.SeDto;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.mamma.enums.ExtraFotosReden;
import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekRedenFotobespreking;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.OnderbrokenOnderzoekOption;
import nl.rivm.screenit.model.mamma.enums.OnvolledigOnderzoekOption;
import nl.rivm.screenit.model.mamma.enums.SuboptimaleInsteltechniek;

@Getter
@Setter
public class OnderzoekSeDto extends SeDto
{

	private Integer eerderMammogramJaartal;

	private Long eerderMammogramZorginstellingId;

	private SuboptimaleInsteltechniek suboptimaleInsteltechniek;

	private MammaOnderzoekRedenFotobespreking redenFotobespreking;

	private Long extraMedewerkerId;

	private String opmerkingMbber;

	private String opmerkingVoorRadioloog;

	private boolean operatieRechts;

	private boolean operatieLinks;

	private MammaAmputatie amputatie;

	private String aanvullendeInformatieOperatie;

	private MammaOnderzoekStatus status;

	private OnvolledigOnderzoekOption onvolledigOnderzoek;

	private OnderbrokenOnderzoekOption onderbrokenOnderzoek;

	private List<ExtraFotosReden> extraFotosRedenen;

	private MammaOnderzoekType onderzoekType;

	private String adviesHuisarts;
}
