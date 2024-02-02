package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.adhoc;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaBeTabelCounterPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.werklijst.MammaOnderzoekMiniWerklijstDataProvider;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaAdhocMeekijkverzoekMiniWerklijstPanel extends Panel
{

	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	@SpringBean
	private HibernateService hibernateService;

	public MammaAdhocMeekijkverzoekMiniWerklijstPanel(String id, AbstractMammaBeoordelenPage parent, Long huidigeOnderzoekId, List<Long> onderzoekenIds)
	{
		super(id);

		MammaOnderzoekMiniWerklijstDataProvider<MammaAdhocMeekijkverzoek> miniWerklijstDataProvider = new MammaOnderzoekMiniWerklijstDataProvider<>(
			huidigeOnderzoekId, onderzoekenIds, MammaAdhocMeekijkverzoek.class);

		List<IColumn<MammaAdhocMeekijkverzoek, String>> columns = new ArrayList<>();

		columns.add(new DateTimePropertyColumn<>(Model.of("Onderzoeksdatum"), "onderzoek.creatieDatum", Constants.getDateTimeFormat()));
		columns.add(new PropertyColumn<>(Model.of("Volgnummer"), "volgnummer"));
		if (ScreenitSession.get().getInstelling().getOrganisatieType() != OrganisatieType.KWALITEITSPLATFORM)
		{
			columns.add(new ClientColumn<>("onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client"));
			columns.add(new GeboortedatumColumn<>("onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon"));
		}
		columns.add(new PropertyColumn<>(Model.of("Reden"), "reden"));
		columns.add(new PropertyColumn<>(Model.of("SE"), "onderzoek.screeningsEenheid.naam"));
		columns.add(new EnumPropertyColumn<>(Model.of("Status"), "status", this));

		addOrReplace(new ScreenitDataTable<MammaAdhocMeekijkverzoek, String>("miniwerklijst", columns, miniWerklijstDataProvider, 5, Model.of("onderzoek(en)"), false)
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaAdhocMeekijkverzoek> model)
			{
				MammaAdhocMeekijkverzoek verzoek = model.getObject();
				parent.gaNaarBeoordeling(verzoek.getOnderzoek().getId(), target);
			}

			@Override
			protected String getCssClass(int index, IModel<MammaAdhocMeekijkverzoek> rowModel)
			{
				if (getDataProvider() instanceof MammaOnderzoekMiniWerklijstDataProvider)
				{
					int openVerslag = ((MammaOnderzoekMiniWerklijstDataProvider) getDataProvider()).getOpenVerslag();

					if (index == openVerslag)
					{
						return "open";
					}
				}
				return super.getCssClass(index, rowModel);
			}

			@Override
			public Panel getCustomPanel(String id)
			{
				IModel<Integer> aantalGezienModel = new IModel<Integer>()
				{
					@Override
					public Integer getObject()
					{
						return kwaliteitscontroleService.getAantalGezienAdhocMeekijkverzoekOnderzoekenInList(onderzoekenIds);
					}
				};

				IModel<Integer> nogTeGaanModel = new IModel<Integer>()
				{
					@Override
					public Integer getObject()
					{
						return onderzoekenIds.size() - aantalGezienModel.getObject();
					}
				};

				return new MammaBeTabelCounterPanel(id, nogTeGaanModel, aantalGezienModel);
			}
		});
	}

}
