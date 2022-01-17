package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.fotobespreking;

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
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaFotobesprekingOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaFotobesprekingMiniWerklijstPanel extends Panel
{

	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	@SpringBean
	private HibernateService hibernateService;

	public MammaFotobesprekingMiniWerklijstPanel(String id, AbstractMammaBeoordelenPage parent, Long huidigeBeoordelingId, List<Long> beoordelingenIds)
	{
		super(id);

		MammaOnderzoekMiniWerklijstDataProvider<MammaFotobesprekingOnderzoek> miniWerklijstDataProvider = new MammaOnderzoekMiniWerklijstDataProvider<>(huidigeBeoordelingId,
			beoordelingenIds, MammaFotobesprekingOnderzoek.class);

		List<IColumn<MammaFotobesprekingOnderzoek, String>> columns = new ArrayList<>();
		columns.add(new DateTimePropertyColumn<>(Model.of("Onderzoeksdatum"), "beoordeling.onderzoek.creatieDatum", Constants.getDateTimeFormat()));
		if (MammobridgeRole.anoniemeRollen().contains(ScreenitSession.get().getMammaHuidigeIDS7Role())
			&& ScreenitSession.get().getInstelling().getOrganisatieType() == OrganisatieType.BEOORDELINGSEENHEID)
		{
			columns.add(new PropertyColumn<>(Model.of("Volgnummer"), "volgnummer"));
		}
		else
		{
			columns.add(new ClientColumn<>("beoordeling.onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client"));
			columns.add(new GeboortedatumColumn<>("beoordeling.onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon"));
		}
		columns.add(new EnumPropertyColumn<>(Model.of("Status"), "status", this));

		addOrReplace(new ScreenitDataTable<MammaFotobesprekingOnderzoek, String>("miniwerklijst", columns, miniWerklijstDataProvider, 5, Model.of("onderzoek(en)"), false)
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaFotobesprekingOnderzoek> model)
			{
				MammaFotobesprekingOnderzoek onderzoek = model.getObject();
				MammaBeoordeling beoordeling = onderzoek.getBeoordeling();
				parent.gaNaarVerslag(beoordeling.getId(), target);
			}

			@Override
			protected String getCssClass(int index, IModel<MammaFotobesprekingOnderzoek> rowModel)
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
				IModel<Integer> besprokenModel = new IModel<Integer>()
				{
					@Override
					public Integer getObject()
					{
						MammaFotobesprekingOnderzoek fotobesprekingOnderzoek = hibernateService.load(MammaFotobesprekingOnderzoek.class, huidigeBeoordelingId);
						return kwaliteitscontroleService.getAantalBesproken(fotobesprekingOnderzoek.getFotobespreking());
					}
				};

				IModel<Integer> teBesprokenModel = new IModel<Integer>()
				{
					@Override
					public Integer getObject()
					{
						return beoordelingenIds.size() - besprokenModel.getObject();
					}
				};

				return new MammaBeTabelCounterPanel(id, teBesprokenModel, besprokenModel);
			}
		});
	}

}
