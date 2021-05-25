package nl.rivm.screenit.mamma.se.service.dtomapper;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.dto.onderzoek.OnderzoekSeDto;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;

import java.util.ArrayList;

class OnderzoekDtoMapper
{
	public OnderzoekSeDto createOnderzoekDto(MammaOnderzoek onderzoek)
	{
		if (onderzoek != null)
		{
			OnderzoekSeDto onderzoekDto = new OnderzoekSeDto();
			onderzoekDto.setId(onderzoek.getId());
			onderzoekDto.setEerderMammogramJaartal(onderzoek.getEerderMammogramJaartal());
			onderzoekDto.setEerderMammogramZorginstellingId(onderzoek.getEerderMammogramZorginstelling() != null ? onderzoek.getEerderMammogramZorginstelling().getId() : null);
			onderzoekDto.setSuboptimaleInsteltechniek(onderzoek.getSuboptimaleInsteltechniek());
			onderzoekDto.setRedenFotobespreking(onderzoek.getRedenFotobespreking());
			onderzoekDto.setOpmerkingMbber(onderzoek.getOpmerkingMbber());
			onderzoekDto.setOpmerkingVoorRadioloog(onderzoek.getOpmerkingVoorRadioloog());
			onderzoekDto.setOperatieRechts(onderzoek.getOperatieRechts());
			onderzoekDto.setOperatieLinks(onderzoek.getOperatieLinks());
			onderzoekDto.setAmputatie(onderzoek.getAmputatie());
			onderzoekDto.setAanvullendeInformatieOperatie(onderzoek.getAanvullendeInformatieOperatie());
			onderzoekDto.setStatus(onderzoek.getStatus());
			onderzoekDto.setOnvolledigOnderzoek(onderzoek.getOnvolledigOnderzoek());
			onderzoekDto.setOnderbrokenOnderzoek(onderzoek.getOnderbrokenOnderzoek());
			onderzoekDto.setExtraFotosRedenen(new ArrayList<>(onderzoek.getExtraFotosRedenen()));
			onderzoekDto.setAdviesHuisarts(onderzoek.getAdviesHuisarts());
			return onderzoekDto;
		}
		else
		{
			return null;
		}
	}
}
