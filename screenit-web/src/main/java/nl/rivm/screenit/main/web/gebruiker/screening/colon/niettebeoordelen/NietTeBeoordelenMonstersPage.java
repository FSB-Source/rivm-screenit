package nl.rivm.screenit.main.web.gebruiker.screening.colon.niettebeoordelen;

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

import java.util.Arrays;

import nl.rivm.screenit.main.service.colon.ColonDossierService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ZoekIfobtMetBarcodePanel;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.ColonScreeningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.RedenNietTeBeoordelen;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.wicket.hibernate.CglibHibernateModel;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_SCREENING_NIETTEBEOORDELEN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class NietTeBeoordelenMonstersPage extends ColonScreeningBasePage
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private ColonDossierService colonDossierService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private InstellingService instellingService;

	private final IModel<IFOBTTest> ifobtTestModel = new CglibHibernateModel<>();

	private final WebMarkupContainer uitnodigingContainer;

	private final ZoekIfobtMetBarcodePanel panel;

	public NietTeBeoordelenMonstersPage()
	{
		panel = new ZoekIfobtMetBarcodePanel("scanForIfobttest")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void ifobtFound(IFOBTTest ifobtTest, AjaxRequestTarget target)
			{
				super.ifobtFound(ifobtTest, target);
				if (ifobtTest != null)
				{
					if (ifobtTest.getUitslag() == null && IFOBTTestStatus.ACTIEF.equals(ifobtTest.getStatus()) && ifobtTest.getType().equals(IFOBTType.GOLD))
					{
						Instelling ingelogdVoorInstelling = ScreenitSession.get().getInstelling();
						if (ingelogdVoorInstelling.getOrganisatieType().equals(OrganisatieType.LABORATORIUM))
						{
							ifobtTest.setIfobtLaboratorium((IFobtLaboratorium) ingelogdVoorInstelling);
						}
						ifobtTestModel.setObject(ifobtTest);
						target.add(uitnodigingContainer);
					}
					else
					{
						error("Test is geen FIT of is al beoordeeld of opnieuw aangevraagd.");
					}
				}
				else
				{
					error("Geen test gevonden bij barcode.");
				}

			}

		};
		add(panel);
		uitnodigingContainer = new WebMarkupContainer("uitnodigingContainer", new CompoundPropertyModel<>(ifobtTestModel))
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onConfigure()
			{
				setVisible(ModelUtil.nullSafeGet(ifobtTestModel) != null);
				super.onConfigure();
			}

		};
		uitnodigingContainer.setOutputMarkupPlaceholderTag(true);
		add(uitnodigingContainer);

		createStatusForm();
	}

	private void createStatusForm()
	{
		Form<Void> statusForm = new Form<>("statusForm");
		uitnodigingContainer.add(statusForm);

		statusForm.add(new AjaxLink<Void>("opnieuw")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				reset(target);
			}
		});

		statusForm.add(
			new ScreenitDropdown<RedenNietTeBeoordelen>("redenNietTeBeoordelen", Arrays.asList(RedenNietTeBeoordelen.values()), new EnumChoiceRenderer<RedenNietTeBeoordelen>())
				.setNullValid(true).setRequired(true));

		Instelling ingelogdVoorInstelling = ScreenitSession.get().getInstelling();
		statusForm.add(new ScreenitDropdown<IFobtLaboratorium>( 
			"ifobtLaboratorium", 
			new SimpleListHibernateModel<>(instellingService.getActieveInstellingen(IFobtLaboratorium.class)), 
			new ChoiceRenderer<IFobtLaboratorium>("naam", "id") 
		) 
			.setNullValid(false).setRequired(true).setEnabled(!ingelogdVoorInstelling.getOrganisatieType().equals(OrganisatieType.LABORATORIUM)));

		statusForm.add(new AjaxSubmitLink("opslaan")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				colonDossierService.monsterNietBeoordeelbaar(ModelProxyHelper.deproxy(ifobtTestModel.getObject()));
				logaction(LogGebeurtenis.IFOBT_NIET_BEOORDEELBAAR, ifobtTestModel.getObject());
				reset(target);
				info(getString("message.gegevensopgeslagen"));
			}
		});

		statusForm.add(new AjaxLink<Void>("annuleren")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				reset(target);
			}
		});
	}

	private void reset(AjaxRequestTarget target)
	{
		panel.reset(target);
		ifobtTestModel.setObject(null);
		target.add(uitnodigingContainer);
	}

	private void logaction(LogGebeurtenis gebeurtenis, IFOBTTest ifobt)
	{
		Account account = ScreenitSession.get().getLoggedInAccount();
		Client client = ifobt.getColonScreeningRonde().getDossier().getClient();
		logService.logGebeurtenis(gebeurtenis, account, client, Bevolkingsonderzoek.COLON);
	}
}
