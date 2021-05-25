package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.route;

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

import nl.rivm.screenit.main.dao.mamma.MammaScreeningsEenheidDao;
import nl.rivm.screenit.main.service.mamma.MammaRouteService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.ExportToXslLink;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaPlanningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.dashboard.MammaPlanningDashboardPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;
import org.wicketstuff.wiquery.core.javascript.JsStatement;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaRoutePage extends MammaPlanningBasePage
{

	private static final long serialVersionUID = 1L;

	private BootstrapDialog dialog;

	private AjaxLink<Void> standplaats;

	private AjaxLink<Void> openSplitview;

	private AjaxLink<Void> closeSplitview;

	private IModel<MammaScreeningsEenheid> screeningsEenheidModel1;

	private IModel<MammaScreeningsEenheid> screeningsEenheidModel2 = new SimpleHibernateModel<>();

	private IModel<List<MammaScreeningsEenheid>> screeningsEenhedenModel1;

	private IModel<List<MammaScreeningsEenheid>> screeningsEenhedenModel2;

	private ScreenitDropdown<MammaScreeningsEenheid> seDropdown1;

	private ScreenitDropdown<MammaScreeningsEenheid> seDropdown2;

	private WebMarkupContainer screeningsEenheid1Container = new WebMarkupContainer("screeningsEenheid1Container");

	private WebMarkupContainer screeningsEenheid2Container = new WebMarkupContainer("screeningsEenheid2Container");

	private final WebMarkupContainer routeContainer;

	private ExportToXslLink exportToXslLink;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private MammaRouteService mammaRouteService;

	@SpringBean
	private MammaScreeningsEenheidDao screeningsEenheidDao;

	public MammaRoutePage(MammaScreeningsEenheid screeningsEenheidInit)
	{
		ScreeningOrganisatie sessionSO = ScreenitSession.get().getScreeningOrganisatie();

		this.screeningsEenheidModel1 = ModelUtil.cModel(screeningsEenheidInit);
		List<MammaScreeningsEenheid> actieveScreeningsEenheden = screeningsEenheidDao.getActieveScreeningsEenhedenVoorScreeningOrganisatie(sessionSO);
		this.screeningsEenhedenModel1 = ModelUtil.listModel(actieveScreeningsEenheden);

		addSEDropdown1();
		List<MammaScreeningsEenheid> screeningsEenheidList2 = new ArrayList<>(actieveScreeningsEenheden);
		screeningsEenheidList2.remove(screeningsEenheidInit);
		this.screeningsEenhedenModel2 = ModelUtil.listModel(screeningsEenheidList2);
		addOrReplaceSEDropdown2();

		addNavigation();

		routeContainer = new WebMarkupContainer("routeContainer");
		routeContainer.setOutputMarkupId(true);
		add(routeContainer);

		refreshRouteTable(null);

		exportToXslLink = new ExportToXslLink("export",
			"Overzicht_route_" + screeningsEenheidModel1.getObject().getNaam() + "_" + currentDateSupplier.getLocalDateTime().format(DateUtil.LOCAL_DATE_TIME_FORMAT))
		{
			@Override
			protected String getCsv()
			{
				return mammaRouteService.getCsvString(screeningsEenheidModel1.getObject());
			}
		};
		exportToXslLink.setOutputMarkupId(true);
		exportToXslLink.setOutputMarkupPlaceholderTag(true);
		add(exportToXslLink);
	}

	private void addSEDropdown1()
	{
		screeningsEenheid1Container.setOutputMarkupId(true);
		add(screeningsEenheid1Container);

		seDropdown1 = new ScreenitDropdown<>("screeningsEenheid1", screeningsEenheidModel1, screeningsEenhedenModel1,
			new ChoiceRenderer<>("naam"));
		seDropdown1.setOutputMarkupId(true);
		screeningsEenheid1Container.add(seDropdown1);
		seDropdown1.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				ScreeningOrganisatie sessionSO = ScreenitSession.get().getScreeningOrganisatie();
				List<MammaScreeningsEenheid> screeningsEenheidList2 = screeningsEenheidDao.getActieveScreeningsEenhedenVoorScreeningOrganisatie(sessionSO);
				screeningsEenheidList2.remove(screeningsEenheidModel1.getObject());
				screeningsEenhedenModel2.setObject(screeningsEenheidList2);
				target.add(screeningsEenheid2Container);
				refreshRouteTable(target);
				controleerBeoordelingsEenheden();
			}
		});
	}

	private void controleerBeoordelingsEenheden()
	{
		MammaScreeningsEenheid screeningsEenheid1 = screeningsEenheidModel1.getObject();
		MammaScreeningsEenheid screeningsEenheid2 = screeningsEenheidModel2.getObject();

		if (magAanpassen && screeningsEenheid2Container.isVisible() && screeningsEenheid1 != null && screeningsEenheid2 != null)
		{
			BeoordelingsEenheid beoordelingsEenheid1 = screeningsEenheid1.getBeoordelingsEenheid();
			BeoordelingsEenheid beoordelingsEenheid2 = screeningsEenheid2.getBeoordelingsEenheid();
			if (!beoordelingsEenheid1.equals(beoordelingsEenheid2))
			{
				warn(String.format("Let op: '%s' behoort to beoordelingseenheid '%s' en '%s' tot beoordelingseenheid '%s'. " +
					"Wisselen van standplaatsen betekend een verschuiving van aantallen cliënten naar een andere beoordelingseenheid, mogelijk is dat niet gewenst.",
					screeningsEenheid1.getNaam(), beoordelingsEenheid1.getNaam(), screeningsEenheid2.getNaam(), beoordelingsEenheid2.getNaam()));
			}
		}
	}

	private void addOrReplaceSEDropdown2()
	{
		screeningsEenheid2Container.setOutputMarkupId(true);
		screeningsEenheid2Container.setOutputMarkupPlaceholderTag(true);
		screeningsEenheid2Container.setVisible(false);
		add(screeningsEenheid2Container);

		seDropdown2 = new ScreenitDropdown<>("screeningsEenheid2", screeningsEenheidModel2, screeningsEenhedenModel2,
			new ChoiceRenderer<>("naam"));
		seDropdown2.setOutputMarkupId(true);
		seDropdown2.setNullValid(true);
		screeningsEenheid2Container.add(seDropdown2);
		seDropdown2.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				ScreeningOrganisatie sessionSO = ScreenitSession.get().getScreeningOrganisatie();
				List<MammaScreeningsEenheid> screeningsEenheidList1 = screeningsEenheidDao.getActieveScreeningsEenhedenVoorScreeningOrganisatie(sessionSO);
				screeningsEenheidList1.remove(screeningsEenheidModel2.getObject());
				screeningsEenhedenModel1.setObject(screeningsEenheidList1);
				target.add(screeningsEenheid1Container);
				refreshRouteTable(target);
				controleerBeoordelingsEenheden();
			}
		});
	}

	private void addNavigation()
	{
		dialog = new BootstrapDialog("dialog");
		add(dialog);

		standplaats = new AjaxLink<Void>("standplaats")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialog.openWith(target, new MammaStandplaatsenToevoegenRoutePanel(IDialog.CONTENT_ID, screeningsEenheidModel1)
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected void standplaatsenToegevoegd(AjaxRequestTarget target)
					{
						dialog.close(target);
						refreshRouteTable(target);
					}

					@Override
					protected void close(AjaxRequestTarget target)
					{
						dialog.close(target);
					}
				});
			}
		};
		standplaats.setVisible(magAanpassen);
		standplaats.setOutputMarkupId(true);
		standplaats.setOutputMarkupPlaceholderTag(true);
		add(standplaats);

		openSplitview = new AjaxLink<Void>("openSplitview")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				openSplitview.setVisible(false);
				target.add(openSplitview);
				closeSplitview.setVisible(true);
				target.add(closeSplitview);

				exportToXslLink.setVisible(false);
				target.add(exportToXslLink);

				screeningsEenheid2Container.setVisible(true);
				target.add(screeningsEenheid2Container);
				refreshRouteTable(target);

				if (magAanpassen)
				{
					standplaats.setVisible(false);
					target.add(standplaats);
				}
			}
		};
		openSplitview.setOutputMarkupId(true);
		openSplitview.setOutputMarkupPlaceholderTag(true);
		add(openSplitview);

		closeSplitview = new AjaxLink<Void>("closeSplitview")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				openSplitview.setVisible(true);
				target.add(openSplitview);
				closeSplitview.setVisible(false);
				target.add(closeSplitview);

				exportToXslLink.setVisible(true);
				target.add(exportToXslLink);

				screeningsEenheid2Container.setVisible(false);
				List<MammaScreeningsEenheid> screeningsEenheidList1 = screeningsEenhedenModel1.getObject();
				if (screeningsEenheidModel2.getObject() != null)
				{
					screeningsEenheidList1.add(screeningsEenheidModel2.getObject());
					screeningsEenheidModel2.setObject(null);
				}
				target.add(screeningsEenheid2Container);
				target.add(screeningsEenheid1Container);

				refreshRouteTable(target);

				if (magAanpassen)
				{
					standplaats.setVisible(true);
					target.add(standplaats);
				}
			}
		};
		closeSplitview.setVisible(false);
		closeSplitview.setOutputMarkupId(true);
		closeSplitview.setOutputMarkupPlaceholderTag(true);
		add(closeSplitview);

		add(new AjaxLink<Void>("terug")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(MammaPlanningDashboardPage.class);
			}
		});
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		JsStatement jsStatement = new JsStatement();
		jsStatement.append("$('.paginering').hide()");
		response.render(OnDomReadyHeaderItem.forScript(jsStatement.render()));
	}

	private void refreshRouteTable(AjaxRequestTarget target)
	{
		MammaStandplaatsPeriodesPanel standplaatsPeriodesPanel;
		if (!screeningsEenheid2Container.isVisible())
		{
			standplaatsPeriodesPanel = new MammaStandplaatsPeriodesPanel("sortableConnectableRouteTable", screeningsEenheidModel1, dialog);
		}
		else
		{
			if (seDropdown2.getModelObject() == null)
			{
				standplaatsPeriodesPanel = new MammaStandplaatsPeriodesPanel("sortableConnectableRouteTable", screeningsEenheidModel1, null, dialog);
			}
			else
			{
				standplaatsPeriodesPanel = new MammaStandplaatsPeriodesPanel("sortableConnectableRouteTable", screeningsEenheidModel1, screeningsEenheidModel2, dialog);
			}
		}
		routeContainer.addOrReplace(standplaatsPeriodesPanel);

		if (target != null)
		{
			target.add(routeContainer);
		}
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return false;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(screeningsEenheidModel1);
		ModelUtil.nullSafeDetach(screeningsEenheidModel2);
		ModelUtil.nullSafeDetach(screeningsEenhedenModel1);
		ModelUtil.nullSafeDetach(screeningsEenhedenModel2);
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveContextMenuClass()
	{
		return MammaPlanningDashboardPage.class;
	}

}
