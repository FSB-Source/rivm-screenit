
package nl.rivm.screenit.main.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Arrays;
import java.util.Date;

import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.cervix.CervixHpvBeoordeling;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class ScreeningRondeGebeurtenis implements IDetachable
{

	private TypeGebeurtenis gebeurtenis;

	private String[] extraOmschrijving = null;

	private Date datum;

	private IModel<InpakbareUitnodiging<?>> uitnodiging;

	private IModel<IFOBTTest> buis;

	private ScreeningRondeGebeurtenissen screeningRondeGebeurtenissen;

	private IModel<ScreeningRonde<?, ?, ?, ?>> screeningRonde;

	private IModel<Verslag<?, ?>> verslag;

	private IModel<ColonIntakeAfspraak> afspraak;

	private IModel<CervixHuisartsBericht> huisartsBericht;

	private IModel<CervixHpvBeoordeling> beoordeling;

	private IModel<ClientBrief<?, ?, ?>> brief;

	private GebeurtenisBron bron;

	private boolean clickable = true;

	public TypeGebeurtenis getGebeurtenis()
	{
		return gebeurtenis;
	}

	public void setGebeurtenis(TypeGebeurtenis gebeurtenis)
	{
		this.gebeurtenis = gebeurtenis;
		clickable = gebeurtenis.getDetailPanelClass() != null;
	}

	public Date getDatum()
	{
		return datum;
	}

	public void setDatum(Date datum)
	{
		this.datum = datum;
	}

	public InpakbareUitnodiging<?> getUitnodiging()
	{
		return ModelUtil.nullSafeGet(uitnodiging);
	}

	public void setUitnodiging(InpakbareUitnodiging<?> uitnodiging)
	{
		this.uitnodiging = ModelUtil.sModel(uitnodiging);
	}

	public IFOBTTest getBuis()
	{
		return ModelUtil.nullSafeGet(buis);
	}

	public void setBuis(IFOBTTest buis)
	{
		this.buis = ModelUtil.sModel(buis);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(uitnodiging);
		ModelUtil.nullSafeDetach(buis);
		ModelUtil.nullSafeDetach(verslag);
		ModelUtil.nullSafeDetach(afspraak);
		ModelUtil.nullSafeDetach(brief);
		ModelUtil.nullSafeDetach(beoordeling);
		ModelUtil.nullSafeDetach(huisartsBericht);
		ModelUtil.nullSafeDetach(screeningRonde);
	}

	public ScreeningRondeGebeurtenissen getScreeningRondeGebeurtenissen()
	{
		return screeningRondeGebeurtenissen;
	}

	public void setScreeningRondeGebeurtenissen(ScreeningRondeGebeurtenissen screeningsronde)
	{
		this.screeningRondeGebeurtenissen = screeningsronde;
	}

	public void setVerslag(Verslag<?, ?> verslag)
	{
		this.verslag = ModelUtil.sModel(verslag);
	}

	public Verslag<?, ?> getVerslag()
	{
		return ModelUtil.nullSafeGet(verslag);
	}

	public ColonIntakeAfspraak getAfspraak()
	{
		return ModelUtil.nullSafeGet(afspraak);
	}

	public void setAfspraak(ColonIntakeAfspraak afspraak)
	{
		this.afspraak = ModelUtil.sModel(afspraak);
	}

	public String[] getExtraOmschrijving()
	{
		return extraOmschrijving;
	}

	public void setExtraOmschrijving(String... extraOmschrijving)
	{
		this.extraOmschrijving = extraOmschrijving;
	}

	public void addToExtraOmschrijving(String... newValues)
	{
		if (extraOmschrijving != null)
		{
			int oldLength = extraOmschrijving.length;
			extraOmschrijving = Arrays.copyOf(extraOmschrijving, oldLength + newValues.length);

			for (int indexInNewValues = 0; indexInNewValues < newValues.length; indexInNewValues++)
			{
				extraOmschrijving[oldLength + indexInNewValues] = newValues[indexInNewValues];
			}
		}
		else
		{
			setExtraOmschrijving(newValues);
		}
	}

	public ClientBrief<?, ?, ?> getBrief()
	{
		return ModelUtil.nullSafeGet(brief);
	}

	public void setBrief(ClientBrief<?, ?, ?> brief)
	{
		this.brief = ModelUtil.sModel(brief);
	}

	public GebeurtenisBron getBron()
	{
		return bron;
	}

	public void setBron(GebeurtenisBron bron)
	{
		this.bron = bron;
	}

	public CervixHpvBeoordeling getBeoordeling()
	{
		return ModelUtil.nullSafeGet(beoordeling);
	}

	public void setBeoordeling(IModel<CervixHpvBeoordeling> beoordeling)
	{
		this.beoordeling = beoordeling;
	}

	public void setBeoordeling(CervixHpvBeoordeling beoordeling)
	{
		this.beoordeling = ModelUtil.sModel(beoordeling);
	}

	public boolean isClickable()
	{
		return clickable;
	}

	public void setClickable(boolean clickable)
	{
		if (gebeurtenis == null || gebeurtenis.getDeclaringClass() != null || !clickable)
		{
			this.clickable = clickable;
		}
	}

	public CervixHuisartsBericht getHuisartsBericht()
	{
		return ModelUtil.nullSafeGet(huisartsBericht);
	}

	public void setHuisartsBericht(CervixHuisartsBericht huisartsBericht)
	{
		this.huisartsBericht = ModelUtil.sModel(huisartsBericht);
	}

	@SuppressWarnings("unchecked")
	public <SR extends ScreeningRonde<?, ?, ?, ?>> SR getScreeningsRonde()
	{
		return (SR) ModelUtil.nullSafeGet(screeningRonde);
	}

	public void setScreeningsRonde(ScreeningRonde<?, ?, ?, ?> screeningsRonde)
	{
		this.screeningRonde = ModelUtil.sModel(screeningsRonde);
	}
}
