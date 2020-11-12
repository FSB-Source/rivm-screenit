package nl.rivm.screenit.dto.mamma.afspraken;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;

public interface IMammaAfspraakWijzigenFilter
{
	boolean getBuitenRegio();

	Date getVanaf();

	LocalDate getVanafLocalDate();

	Date getTotEnMet();

	LocalDate getTotEnMetLocalDate();

	List<MammaStandplaats> getStandplaatsen();

	List<MammaScreeningsEenheid> getScreeningsEenheden();

	String getPlaats();

	Integer getAfstand();

	Boolean getExtraOpties();

	void setBuitenRegio(boolean buitenRegio);

	void setVanaf(Date vanaf);

	void setTotEnMet(Date totEnMet);

	void setStandplaatsen(List<MammaStandplaats> standplaatsen);

	void setScreeningsEenheden(List<MammaScreeningsEenheid> screeningsEenheden);

	void setPlaats(String plaats);

	void setAfstand(Integer afstand);

	void setExtraOpties(Boolean extraOpties);

	Client getClient();

	MammaVerzettenReden getVerzettenReden();
}
