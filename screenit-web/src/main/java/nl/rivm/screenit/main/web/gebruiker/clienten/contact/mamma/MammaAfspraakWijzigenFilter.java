package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class MammaAfspraakWijzigenFilter implements IMammaAfspraakWijzigenFilter, IDetachable
{

	private LocalDate vanaf;

	private LocalDate totEnMet;

	private IModel<List<MammaStandplaats>> standplaatsenModel = ModelUtil.listRModel(new ArrayList<>());

	private IModel<List<MammaScreeningsEenheid>> screeningsEenhedenModel = ModelUtil.listRModel(new ArrayList<>());

	private IModel<Client> client;

	private String plaats;

	private Integer afstand;

	private Boolean extraOpties = false;

	private boolean buitenRegio = false;

	private MammaVerzettenReden verzettenReden;

	public MammaAfspraakWijzigenFilter(LocalDate vanaf, LocalDate totEnMet, MammaStandplaats standplaats, MammaScreeningsEenheid screeningsEenheid)
	{
		setVanaf(vanaf);
		setTotEnMet(totEnMet);
		if (standplaats != null)
		{
			this.standplaatsenModel.getObject().add(standplaats);
		}
		if (screeningsEenheid != null)
		{
			this.screeningsEenhedenModel.getObject().add(screeningsEenheid);
		}
	}

	@Override
	public boolean isBuitenRegio()
	{
		return buitenRegio;
	}

	@Override
	public LocalDate getVanaf()
	{
		return vanaf;
	}
	@Override
	public LocalDate getTotEnMet()
	{
		return totEnMet;
	}

	@Override
	public List<MammaStandplaats> getStandplaatsen()
	{
		return standplaatsenModel.getObject();
	}

	@Override
	public List<MammaScreeningsEenheid> getScreeningsEenheden()
	{
		return screeningsEenhedenModel.getObject();
	}

	@Override
	public String getPlaats()
	{
		return plaats;
	}

	@Override
	public Integer getAfstand()
	{
		return afstand;
	}

	@Override
	public Boolean getExtraOpties()
	{
		return extraOpties;
	}

	@Override
	public void setBuitenRegio(boolean buitenRegio)
	{
		this.buitenRegio = buitenRegio;
	}

	@Override
	public void setVanaf(LocalDate vanaf)
	{
		this.vanaf = vanaf;
	}

	@Override
	public void setTotEnMet(LocalDate totEnMet)
	{
		this.totEnMet = totEnMet;
	}

	@Override
	public void setStandplaatsen(List<MammaStandplaats> standplaatsen)
	{
		this.standplaatsenModel.setObject(standplaatsen);
	}

	@Override
	public void setScreeningsEenheden(List<MammaScreeningsEenheid> screeningsEenheden)
	{
		this.screeningsEenhedenModel.setObject(screeningsEenheden);
	}

	@Override
	public void setPlaats(String plaats)
	{
		this.plaats = plaats;
	}

	@Override
	public void setAfstand(Integer afstand)
	{
		this.afstand = afstand;
	}

	@Override
	public void setExtraOpties(Boolean extraOpties)
	{
		this.extraOpties = extraOpties;
	}

	@Override
	public Client getClient()
	{
		return ModelUtil.nullSafeGet(client);
	}

	public void setClient(Client client)
	{
		this.client = ModelUtil.sModel(client);
	}

	@Override
	public MammaVerzettenReden getVerzettenReden()
	{
		return verzettenReden;
	}

	public void setVerzettenReden(MammaVerzettenReden verzettenReden)
	{
		this.verzettenReden = verzettenReden;
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(standplaatsenModel);
		ModelUtil.nullSafeDetach(screeningsEenhedenModel);
		ModelUtil.nullSafeDetach(client);
	}
}
