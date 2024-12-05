package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaCeWerklijstZoekObject;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.AbstractMammaCePage;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaLezing_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

import static nl.rivm.screenit.util.StringUtil.propertyChain;

public abstract class AbstractMammaCeWerklijst extends AbstractMammaCePage
{

	protected IModel<MammaCeWerklijstZoekObject> zoekObjectModel;

	protected MammaCeVerwijsVerslagenDataProvider onderzoekDataProvider;

	protected final WebMarkupContainer resultatenContainer;

	protected AbstractMammaCeWerklijst()
	{
		if (ScreenitSession.get().isZoekObjectGezetForComponent(getPageClass()))
		{
			zoekObjectModel = (IModel<MammaCeWerklijstZoekObject>) ScreenitSession.get().getZoekObject(getPageClass());
		}
		else
		{
			zoekObjectModel = new CompoundPropertyModel<>(new MammaCeWerklijstZoekObject());
		}
		onderzoekDataProvider = new MammaCeVerwijsVerslagenDataProvider(MammaOnderzoek_.CREATIE_DATUM, zoekObjectModel);

		resultatenContainer = new WebMarkupContainer("resultatenContainer");
		resultatenContainer.setOutputMarkupId(Boolean.TRUE);
		add(resultatenContainer);
	}

	@Override
	protected void onDetach()
	{
		ModelUtil.nullSafeDetach(zoekObjectModel);
		super.onDetach();
	}

	protected IColumn<MammaBeoordeling, String> getOnderzoeksdatumColumn()
	{
		return new DateTimePropertyColumn<>(Model.of("Onderzoeksdatum SE"), "onderzoek.creatieDatum", MammaOnderzoek_.CREATIE_DATUM, Constants.getDateTimeFormat());
	}

	protected IColumn<MammaBeoordeling, String> getStatusColumn()
	{
		return new EnumPropertyColumn<>(Model.of("Status"), propertyChain(MammaOnderzoek_.LAATSTE_BEOORDELING, MammaBeoordeling_.STATUS), "status", this);
	}

	protected IColumn<MammaBeoordeling, String> getBeColumn()
	{
		return new PropertyColumn<>(Model.of("BE"), propertyChain(MammaOnderzoek_.LAATSTE_BEOORDELING, MammaBeoordeling_.BEOORDELINGS_EENHEID, Instelling_.NAAM),
			"beoordelingsEenheid.naam");
	}

	protected IColumn<MammaBeoordeling, String> getSeColumn()
	{
		return new PropertyColumn<>(Model.of("SE"), propertyChain(MammaOnderzoek_.SCREENINGS_EENHEID, MammaScreeningsEenheid_.CODE), "onderzoek.screeningsEenheid.code");
	}

	protected IColumn<MammaBeoordeling, String> getBsnColumn()
	{
		return new PropertyColumn<>(Model.of("BSN"), propertyChain(persoonSortProperty(), GbaPersoon_.BSN),
			"onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon.bsn");
	}

	protected IColumn<MammaBeoordeling, String> getGeboortedatumColumn()
	{
		return new GeboortedatumColumn<>(propertyChain(persoonSortProperty(), GbaPersoon_.GEBOORTEDATUM), "onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon");
	}

	protected IColumn<MammaBeoordeling, String> getClientColumn()
	{
		return new ClientColumn<>(propertyChain(persoonSortProperty(), GbaPersoon_.ACHTERNAAM), "onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client");
	}

	protected IColumn<MammaBeoordeling, String> getVerslagdatumColumn()
	{
		return new DateTimePropertyColumn<>(Model.of("Verslagdatum"), "verslagLezing.beoordelingDatum",
			propertyChain(MammaOnderzoek_.LAATSTE_BEOORDELING, MammaBeoordeling_.VERSLAG_LEZING, MammaLezing_.BEOORDELING_DATUM), Constants.getDateFormat());
	}

	protected IColumn<MammaBeoordeling, String> getTypeOnderzoekColumn()
	{
		return new EnumPropertyColumn<>(Model.of("Type onderzoek"), MammaOnderzoek_.ONDERZOEK_TYPE, "onderzoek.onderzoekType");
	}

	private String persoonSortProperty()
	{
		return propertyChain(rondeSortProperty(), MammaScreeningRonde_.DOSSIER, MammaDossier_.CLIENT, Client_.PERSOON);
	}

	protected String rondeSortProperty()
	{
		return propertyChain(propertyChain(MammaOnderzoek_.AFSPRAAK, MammaAfspraak_.UITNODIGING, MammaUitnodiging_.SCREENING_RONDE));
	}
}
