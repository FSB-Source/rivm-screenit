package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.uploadbeeldenverzoeken;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.mamma.MammaUploadBeeldenService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.NotClickableAbstractColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.ClientDossierPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.AbstractMammaCePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.panel.MammaGeenBeeldenBeschikbaarPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.panel.MammaUploadBeeldenVerzoekAnnulerenPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoek;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_CENTRALE_EENHEID_UPLOADVERZOEKEN },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaCeUploadBeeldenVerzoekInstellingWerklijstPage extends AbstractMammaCePage
{
	@SpringBean
	private MammaUploadBeeldenService uploadBeeldenService;

	private ScreenitDataTable<MammaUploadBeeldenVerzoek, String> table;

	public MammaCeUploadBeeldenVerzoekInstellingWerklijstPage(IModel<Instelling> instellingModel)
	{
		super();
		setDefaultModel(new CompoundPropertyModel<>(instellingModel));
		add(new Label("naam"));
		createWerklijst();
	}

	public void createWerklijst()
	{
		List<IColumn<MammaUploadBeeldenVerzoek, String>> columns = new ArrayList<>();

		columns.add(new ClientColumn<>("screeningRonde.dossier.client"));
		columns.add(new GeboortedatumColumn<>("screeningRonde.dossier.client.persoon"));
		columns.add(new PropertyColumn<>(Model.of("Bsn"), "screeningRonde.dossier.client.persoon.bsn"));
		columns.add(new EnumPropertyColumn<>(Model.of("Verzoektype"), "verzoekType"));
		columns.add(new EnumPropertyColumn<>(Model.of("Datum verzoek"), "creatieDatum", "creatieDatum"));
		columns.add(new EnumPropertyColumn<>(Model.of("Status"), "status", "status", this));
		columns.add(new NotClickableAbstractColumn<MammaUploadBeeldenVerzoek, String>(Model.of(""))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaUploadBeeldenVerzoek>> item, String s, IModel<MammaUploadBeeldenVerzoek> uploadBeeldenVerzoekModel)
			{
				item.add(new MammaGeenBeeldenBeschikbaarPanel(s, dialog, "geenBeeldenBeschikbaar", uploadBeeldenVerzoekModel)
				{
					@Override
					protected void onOpslaan(AjaxRequestTarget ajaxRequestTarget, MammaUploadBeeldenVerzoek uploadBeeldenVerzoek)
					{
						try
						{
							uploadBeeldenService.setGeenBeeldenBeschikbaar(uploadBeeldenVerzoek, ScreenitSession.get().getLoggedInInstellingGebruiker());
							getPage().info(getString("MammaUploadBeeldenVerzoekActie.GEEN_BEELDEN_BESCHIKBAAR"));

						}
						catch (IllegalStateException e)
						{
							getPage().error(getString("MammaUploadBeeldenVerzoekActie.ERROR"));
						}
						finally
						{
							ajaxRequestTarget.add(table);
						}
					}
				});
			}
		});

		columns.add(new NotClickableAbstractColumn<MammaUploadBeeldenVerzoek, String>(Model.of(""))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaUploadBeeldenVerzoek>> item, String s, IModel<MammaUploadBeeldenVerzoek> uploadBeeldenVerzoekModel)
			{
				item.add(new MammaUploadBeeldenVerzoekAnnulerenPanel(s, dialog, "verzoekAnnuleren", uploadBeeldenVerzoekModel)
				{
					@Override
					protected void onOpslaan(AjaxRequestTarget ajaxRequestTarget, MammaUploadBeeldenVerzoek uploadBeeldenVerzoek)
					{
						try
						{
							uploadBeeldenService.annuleerVerzoek(uploadBeeldenVerzoek, ScreenitSession.get().getLoggedInInstellingGebruiker());
							getPage().info(getString("MammaUploadBeeldenVerzoekActie.ANNULEREN"));

						}
						catch (IllegalStateException e)
						{
							getPage().error(getString("MammaUploadBeeldenVerzoekActie.ERROR"));
						}
						finally
						{
							ajaxRequestTarget.add(table);
						}
					}
				});
			}
		});

		MammaCeUploadBeeldenVerzoekInstellingDataProvider dataProvider = new MammaCeUploadBeeldenVerzoekInstellingDataProvider((IModel<Instelling>) getDefaultModel(),
			ModelUtil.sModel(ScreenitSession.get().getScreeningOrganisatie()));
		table = new ScreenitDataTable<MammaUploadBeeldenVerzoek, String>("werklijst", columns, dataProvider, 10,
			Model.of("verzoek(en)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaUploadBeeldenVerzoek> model)
			{
				super.onClick(target, model);
				if (ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(ClientDossierPage.class))
				{
					setResponsePage(new ClientDossierPage(ModelUtil.sModel(model.getObject().getScreeningRonde().getDossier().getClient())));
				}
			}
		};

		add(table);
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveContextMenuClass()
	{
		return MammaCeUploadBeeldenVerzoekWerklijstPage.class;
	}
}
