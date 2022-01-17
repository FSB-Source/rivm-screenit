package nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followupconclusie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.AbstractMammaFollowUpPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.wicketstuff.shiro.ShiroConstraint;

import java.util.ArrayList;
import java.util.List;

@SecurityConstraint(
	constraint = ShiroConstraint.HasPermission,
	checkScope = true,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
	recht = { Recht.GEBRUIKER_MAMMA_FOLLOW_UP_CONCLUSIE_WERKLIJST },
	organisatieTypeScopes = { OrganisatieType.SCREENINGSORGANISATIE })
public class MammaFollowUpConclusieWerklijst extends AbstractMammaFollowUpPage
{
	private WebMarkupContainer refreshContainer;

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		MammaFollowUpConclusieProvider followUpConclusieProvider = new MammaFollowUpConclusieProvider(ModelUtil.sModel(ScreenitSession.get().getScreeningOrganisatie()));

		refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		List<IColumn<MammaBeoordeling, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Onderzoeksdatum SE"), "onderzoek.creatieDatum", "onderzoek.creatieDatum"));
        columns.add(new ClientColumn<>("onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client"));
		columns.add(new PropertyColumn<>(Model.of("Geboortedatum"), "onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon.geboortedatum"));
		columns.add(new PropertyColumn<>(Model.of("Bsn"), "onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon.bsn"));

		ScreenitDataTable<MammaBeoordeling, String> table = new ScreenitDataTable<MammaBeoordeling, String>("resultaten", columns, followUpConclusieProvider, 10,
			Model.of("cliënt(en)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaBeoordeling> model)
			{
				setResponsePage(new MammaFollowUpConclusiePage(model.getObject().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde()));
			}
		};

		refreshContainer.add(table);
	}

}
