package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatiemedewerker;

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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.NavigeerNaarCellPanel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.helper.InstellingGebruikerRolComparator;
import nl.rivm.screenit.security.IScreenitRealm;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.CglibHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.flow.RedirectToUrlException;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_MEDEWERKER_ORGANISATIES_BEHEER,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public abstract class OrganisatieMedewerkerKoppelPage extends AlgemeenPage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private IScreenitRealm realm;

	private boolean magKoppelingenBeheren;

	private Boolean actiefFilter = true;

	public OrganisatieMedewerkerKoppelPage(final boolean showMedewerkers)
	{
		Actie actie = getActie(Recht.GEBRUIKER_MEDEWERKER_ORGANISATIES_BEHEER);

		if (actie == null)
		{

			ScreenitSession.get().logout();
			throw new RedirectToUrlException("/");
		}

		magKoppelingenBeheren = actie.getNiveau() >= Actie.TOEVOEGEN.getNiveau();

		final BootstrapDialog dialog = new BootstrapDialog("dialog");
		add(dialog);
		final WebMarkupContainer medewerkerContainer = new WebMarkupContainer("medewerkerContainer");
		medewerkerContainer.setOutputMarkupId(true);

		medewerkerContainer.add(getPaspoortPanel("paspoort"));

		String organisatieMedewerkerSort = "medewerker.achternaam";
		String organisatieMedewerkerProperty = "medewerker.naamVolledigMetVoornaam";
		if (!showMedewerkers)
		{
			organisatieMedewerkerSort = "organisatie.naam";
			organisatieMedewerkerProperty = "organisatie.naam";
		}

		List<IColumn<InstellingGebruiker, String>> columns = new ArrayList<IColumn<InstellingGebruiker, String>>();
		columns.add(new PropertyColumn<InstellingGebruiker, String>(Model.of("Naam"), organisatieMedewerkerSort, organisatieMedewerkerProperty));
		columns.add(new PropertyColumn<InstellingGebruiker, String>(Model.of("Rollen"), "rollen.naam", "rollen.naam")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<InstellingGebruiker>> item, String componentId, IModel<InstellingGebruiker> rowModel)
			{

				InstellingGebruiker organisatieMedewerker = rowModel.getObject();

				String rollen = "";
				boolean first = true;

				List<InstellingGebruikerRol> rollenLijst = new ArrayList<>();
				rollenLijst = organisatieMedewerker.getRollen();
				InstellingGebruikerRolComparator comparator = new InstellingGebruikerRolComparator();
				Collections.sort(rollenLijst, comparator);

				for (InstellingGebruikerRol organisatieMedewerkerRol : rollenLijst)
				{
					if (isRolActief(organisatieMedewerkerRol))
					{
						if (!first)
						{
							rollen += ", ";
						}
						rollen += organisatieMedewerkerRol.getRol().getNaam();
						if (!organisatieMedewerkerRol.getBevolkingsonderzoeken().isEmpty())
						{
							rollen += " (" + Bevolkingsonderzoek.getAfkortingen(organisatieMedewerkerRol.getBevolkingsonderzoeken()) + ")";
						}
						first = false;
					}
				}
				item.add(new Label(componentId, rollen));
			}

			private boolean isRolActief(InstellingGebruikerRol organisatieMedewerkerRol)
			{
				Calendar today = Calendar.getInstance();
				today.add(Calendar.DATE, -1);
				return !Boolean.FALSE.equals(organisatieMedewerkerRol.getActief())
					&& (organisatieMedewerkerRol.getEindDatum() == null || !today.getTime().after(organisatieMedewerkerRol.getEindDatum()) || !Boolean.TRUE.equals(actiefFilter));
			}

			@Override
			public boolean isSortable()
			{
				return false;
			}

		});
		columns.add(new PropertyColumn<InstellingGebruiker, String>(Model.of("Begindatum"), organisatieMedewerkerProperty)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<InstellingGebruiker>> item, String componentId, IModel<InstellingGebruiker> rowModel)
			{
				InstellingGebruiker organisatieMedewerker = rowModel.getObject();

				Date inDienst = null;
				for (InstellingGebruikerRol organisatieMedewerkerRol : organisatieMedewerker.getRollen())
				{
					if (!Boolean.FALSE.equals(organisatieMedewerkerRol.getActief()))
					{
						if (inDienst == null)
						{
							inDienst = organisatieMedewerkerRol.getBeginDatum();
						}
						else if (organisatieMedewerkerRol.getBeginDatum() != null && organisatieMedewerkerRol.getBeginDatum().before(inDienst))
						{
							inDienst = organisatieMedewerkerRol.getBeginDatum();
						}
					}
				}
				String inDienstDatum = "";
				if (inDienst != null)
				{
					inDienstDatum = new SimpleDateFormat("dd-MM-yyyy").format(inDienst);
				}
				item.add(new Label(componentId, inDienstDatum));
			}

		});
		columns.add(new PropertyColumn<InstellingGebruiker, String>(Model.of("Einddatum"), organisatieMedewerkerProperty)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<InstellingGebruiker>> item, String componentId, IModel<InstellingGebruiker> rowModel)
			{
				InstellingGebruiker organisatieMedewerker = rowModel.getObject();

				Date uitDienst = null;
				boolean isNull = false;
				for (InstellingGebruikerRol organisatieMedewerkerRol : organisatieMedewerker.getRollen())
				{
					if (!Boolean.FALSE.equals(organisatieMedewerkerRol.getActief()))
					{
						if (organisatieMedewerkerRol.getEindDatum() == null)
						{
							isNull = true;
						}

						if (uitDienst == null)
						{
							uitDienst = organisatieMedewerkerRol.getEindDatum();
						}
						else if (organisatieMedewerkerRol.getEindDatum() != null && organisatieMedewerkerRol.getEindDatum().after(uitDienst))
						{
							uitDienst = organisatieMedewerkerRol.getEindDatum();
						}

					}
				}
				String uitDienstDatum = "";
				if (uitDienst != null && !isNull)
				{
					uitDienstDatum = new SimpleDateFormat("dd-MM-yyyy").format(uitDienst);
				}
				item.add(new Label(componentId, uitDienstDatum));
			}

		});

		columns.add(new PropertyColumn<InstellingGebruiker, String>(Model.of(""), null)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<InstellingGebruiker>> item, String componentId, IModel<InstellingGebruiker> rowModel)
			{
				item.add(new NavigeerNaarCellPanel<InstellingGebruiker>(componentId, rowModel)
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected boolean magNavigerenNaar(IModel<InstellingGebruiker> rowModel)
					{
						return OrganisatieMedewerkerKoppelPage.this.magNavigerenNaar(rowModel);
					}

					@Override
					protected void onNavigeerNaar(AjaxRequestTarget target, IModel<InstellingGebruiker> rowModel)
					{
						OrganisatieMedewerkerKoppelPage.this.onNavigeerNaar(rowModel, target);
					}
				});
			}
		});

		InstellingGebruiker instellingGebruiker = createSearchObject();
		OrganisatieType type = null;
		if (instellingGebruiker.getOrganisatie() != null)
		{
			type = instellingGebruiker.getOrganisatie().getOrganisatieType();
		}
		if (type == null && instellingGebruiker.getMedewerker() != null && instellingGebruiker.getMedewerker().getOrganisatieMedewerkers() != null
			&& instellingGebruiker.getMedewerker().getOrganisatieMedewerkers().size() > 0)
		{
			type = instellingGebruiker.getMedewerker().getOrganisatieMedewerkers().get(0).getOrganisatie().getOrganisatieType();
		}
		magKoppelingenBeheren &= !OrganisatieType.HUISARTS.equals(type);
		IModel<InstellingGebruiker> searchObjectModel = new CglibHibernateModel<>(instellingGebruiker);
		columns.add(new ActiefPropertyColumn<InstellingGebruiker, InstellingGebruiker>(Model.of("actief"), "actief", medewerkerContainer, searchObjectModel,
			magKoppelingenBeheren, dialog, "question.remove.organisatiemedewerker")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onChangeActiefFilter(AjaxRequestTarget target, Boolean nieuwActiefFilter)
			{
				target.add(medewerkerContainer);
				actiefFilter = nieuwActiefFilter;
			}

			@Override
			protected void onAfterToggleActief(AjaxRequestTarget target, InstellingGebruiker instellingGebruiker)
			{
				super.onAfterToggleActief(target, instellingGebruiker);
				Gebruiker gebruiker = instellingGebruiker.getMedewerker();
				Instelling instelling = instellingGebruiker.getOrganisatie();
				if (instellingGebruiker.getId() == null)
				{
					gebruiker.getOrganisatieMedewerkers().remove(instellingGebruiker);
					instellingGebruiker.setMedewerker(null);
					instelling.getOrganisatieMedewerkers().remove(instellingGebruiker);
					instellingGebruiker.setOrganisatie(null);
					target.add(medewerkerContainer);
				}
				else
				{
					hibernateService.saveOrUpdate(instellingGebruiker);
					if (Boolean.FALSE.equals(instellingGebruiker.getActief()))
					{
						logService.logGebeurtenis(LogGebeurtenis.ORGANISATIE_MEDEWERKER_ONTKOPPEL, ScreenitSession.get().getLoggedInAccount(),
							String.format("Medewerker %1$s ontkoppeld van organisatie %2$s", gebruiker.getNaamVolledigMetVoornaam(), instelling.getNaam()));
					}
					else
					{
						logService.logGebeurtenis(LogGebeurtenis.ORGANISATIE_MEDEWERKER_KOPPEL, ScreenitSession.get().getLoggedInInstellingGebruiker(),
							String.format("Medewerker %1$s gekoppeld aan organisatie %2$s", gebruiker.getNaamVolledigMetVoornaam(), instelling.getNaam()));
					}
					clearCachedAuthorizationInfo(instellingGebruiker);
				}

			}

		});

		IModel<String> totaalLabel = new Model<>("organisaties");
		if (showMedewerkers)
		{
			totaalLabel = new Model<>("medewerkers");
		}

		ScreenitDataTable<InstellingGebruiker, String> dataTable = new ScreenitDataTable<InstellingGebruiker, String>("organisaties", columns,
			new OrganisatieMedewerkerDataProvider(searchObjectModel, organisatieMedewerkerSort), totaalLabel)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<InstellingGebruiker> model)
			{
				if (magKoppelingenBeheren)
				{
					dialog.setContent(new OrganisatieMedewerkerRollenPanel(IDialog.CONTENT_ID, ModelUtil.cModel(model.getObject()), dialog, medewerkerContainer));
					dialog.add(new AttributeAppender("class", Model.of("dialog-large"), " "));
					dialog.open(target);
				}
			}
		};

		medewerkerContainer.add(dataTable);

		AjaxLink<Void> link = new AjaxLink<Void>("toevoegen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				onToevoegen(dialog, target, medewerkerContainer);
			}
		};
		link.setVisible(magKoppelingenBeheren);
		medewerkerContainer.add(link);

		add(medewerkerContainer);
	}

	public abstract Actie getActie(Recht recht);

	protected abstract Panel getPaspoortPanel(String id);

	protected abstract void onNavigeerNaar(IModel<InstellingGebruiker> rowModel, AjaxRequestTarget target);

	protected abstract boolean magNavigerenNaar(IModel<InstellingGebruiker> rowModel);

	protected InstellingGebruiker createSearchObject()
	{
		InstellingGebruiker instellingGebruiker = new InstellingGebruiker();
		instellingGebruiker.setActief(Boolean.TRUE);
		InstellingGebruikerRol rol = new InstellingGebruikerRol();
		Calendar today = Calendar.getInstance();
		today.add(Calendar.DATE, -1);
		rol.setEindDatum(today.getTime());

		return instellingGebruiker;
	}

	protected abstract void onToevoegen(IDialog dialog, AjaxRequestTarget target, WebMarkupContainer medewerkerContainer);

	protected void clearCachedAuthorizationInfo(InstellingGebruiker instellingGebruiker)
	{
		realm.clearCachedAuthorizationInfo(instellingGebruiker);
	}

}
