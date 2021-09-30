package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.werklijst;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaBeTabelCounterPanel;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MiniWerklijstPanel extends Panel
{

	@SpringBean
	protected MammaBeoordelingService beoordelingService;

	private IModel<MammaBeoordeling> clickedBeoordeling;

	public MiniWerklijstPanel(String id, AbstractMammaBeoordelenPage parent, Long huidigeBeoordelingId, List<Long> beoordelingenIds)
	{
		super(id);

		MammaOnderzoekMiniWerklijstDataProvider<MammaBeoordeling> miniWerklijstDataProvider = new MammaOnderzoekMiniWerklijstDataProvider<>(huidigeBeoordelingId, beoordelingenIds,
			MammaBeoordeling.class);

		List<IColumn<MammaBeoordeling, String>> columns = new ArrayList<>();
		columns.add(new DateTimePropertyColumn<>(Model.of("Onderzoeksdatum"), "onderzoek.creatieDatum", Constants.getDateTimeFormat()));
		columns.add(new ClientColumn<>("onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client"));
		columns.add(new GeboortedatumColumn<>("onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon"));
		columns.add(maakStatusKolom());

		addOrReplace(new ScreenitDataTable<MammaBeoordeling, String>("miniwerklijst", columns, miniWerklijstDataProvider, 5, Model.of("beoordeling(en)"), false)
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaBeoordeling> model)
			{
				clickedBeoordeling = model;
				target.prependJavaScript("checkForChangesBeforeClick('" + getMarkupId() + "')");
			}

			@Override
			protected String getCssClass(int index, IModel<MammaBeoordeling> rowModel)
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
				IModel<Integer> beoordeeldModel = new IModel<Integer>()
				{
					@Override
					public Integer getObject()
					{
						return beoordelingService.getAantalBeoordeeldInList(beoordelingenIds);
					}
				};

				IModel<Integer> teBeoordelenModel = new IModel<Integer>()
				{
					@Override
					public Integer getObject()
					{
						return beoordelingenIds.size() - beoordeeldModel.getObject();
					}
				};

				return new MammaBeTabelCounterPanel(id, teBeoordelenModel, beoordeeldModel);
			}
		}).setOutputMarkupId(true).add(new AjaxEventBehavior("changesConfirmedEvent")
		{
			protected void onEvent(final AjaxRequestTarget target)
			{
				MammaBeoordeling beoordeling = clickedBeoordeling.getObject();
				parent.gaNaarVerslag(beoordeling.getId(), target);
			}
		});

	}

	protected PropertyColumn<MammaBeoordeling, String> maakStatusKolom()
	{
		return new EnumPropertyColumn<>(Model.of("Status"), "status");
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(clickedBeoordeling);
	}
}
