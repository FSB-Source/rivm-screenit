package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.geenbeoordelingmogelijk;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.panels.MammaCeZoekPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.AbstractMammaCeWerklijst;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.TelefoonnrColumn;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.wicketstuff.shiro.ShiroConstraint;

import static nl.rivm.screenit.util.StringUtil.propertyChain;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_CENTRALE_EENHEID_GEEN_BEOORDELING_MOGELIJK },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaCeGeenBeoordelingMogelijkWerklijstPage extends AbstractMammaCeWerklijst
{
	private final MammaCeGeenBeoordelingMogelijkDataProvider geenBeoordelingMogelijkDataProvider;

	public MammaCeGeenBeoordelingMogelijkWerklijstPage()
	{
		super();
		geenBeoordelingMogelijkDataProvider = new MammaCeGeenBeoordelingMogelijkDataProvider(propertyChain(MammaOnderzoek_.LAATSTE_BEOORDELING, MammaBeoordeling_.STATUS_DATUM),
			zoekObjectModel);
		createResultTable();
		MammaCeZoekPanel zoekPanel = new MammaCeZoekPanel("zoekContainer", zoekObjectModel, this, resultatenContainer)
		{

			@Override
			protected List<MammaBeoordelingStatus> getRemoveFromDefaultFilter()
			{
				List<MammaBeoordelingStatus> beoordelingStatussen = new ArrayList<>();
				beoordelingStatussen.add(MammaBeoordelingStatus.ONBEOORDEELBAAR);
				return beoordelingStatussen;
			}

			@Override
			protected List<MammaBeoordelingStatus> getMammaMogelijkeBeoordelingFilterStatussen()
			{
				List<MammaBeoordelingStatus> beoordelingStatussen = new ArrayList<>();
				beoordelingStatussen.add(MammaBeoordelingStatus.ONBEOORDEELBAAR_TE_VERSTUREN);
				beoordelingStatussen.add(MammaBeoordelingStatus.ONBEOORDEELBAAR);
				return beoordelingStatussen;
			}
		};
		add(zoekPanel);
	}

	private void createResultTable()
	{
		List<IColumn<MammaBeoordeling, String>> columns = new ArrayList<>();
		columns.add(getOnderzoeksdatumColumn());
		columns.add(getClientColumn());
		columns.add(getGeboortedatumColumn());
		columns.add(getBsnColumn());
		columns.add(new TelefoonnrColumn<>("onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon"));
		columns.add(getSeColumn());
		columns.add(new PropertyColumn<>(Model.of("CE"),
			propertyChain(MammaOnderzoek_.LAATSTE_BEOORDELING, MammaBeoordeling_.BEOORDELINGS_EENHEID, Instelling_.PARENT, Instelling_.NAAM),
			"onderzoek.screeningsEenheid.beoordelingsEenheid.parent.naam"));

		resultatenContainer.add(new ScreenitDataTable<>("resultaten", columns, geenBeoordelingMogelijkDataProvider, 10,
			Model.of("onderzoek(en) waarbij geen beoordeling mogelijk is"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaBeoordeling> model)
			{
				super.onClick(target, model);
				setResponsePage(new MammaCeGeenBeoordelingMogelijkPage(model));
			}
		});
	}
}
