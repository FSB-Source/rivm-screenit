package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.dto.mamma.afspraken.MammaKandidaatAfspraakDto;
import nl.rivm.screenit.exceptions.MammaTijdNietBeschikbaarException;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaAfspraakKiezenPanel extends AbstractClientContactActiePanel<Client>
{
	private static final long serialVersionUID = 1L;

	private Panel nieuweAfspraakPanel;

	private Panel afspraakZoekenPanel;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private MammaBaseFactory baseFactory;

	public MammaAfspraakKiezenPanel(String id, IModel<Client> clientModel)
	{
		super(id, clientModel);

		nieuweAfspraakPanel = new EmptyPanel("nieuweAfspraakPanel");
		nieuweAfspraakPanel.setOutputMarkupId(true);
		add(nieuweAfspraakPanel);

		afspraakZoekenPanel = new MammaAfspraakZoekenPanel("afspraakZoekenPanel", clientModel)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void nieuweAfspraak(AjaxRequestTarget target, MammaKandidaatAfspraakDto kandidaatAfspraakDto, MammaVerzettenReden verzettenReden)
			{
				if (!checkBestaatCapaciteitBlokNog(target, kandidaatAfspraakDto.getCapaciteitBlokId()))
				{
					return;
				}
				MammaCapaciteitBlok capaciteitBlok = hibernateService.load(MammaCapaciteitBlok.class, kandidaatAfspraakDto.getCapaciteitBlokId());
				MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class, kandidaatAfspraakDto.getStandplaatsPeriodeId());
				MammaUitnodiging uitnodiging = getModelObject().getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging();

				Date vanaf = DateUtil.toUtilDate(kandidaatAfspraakDto.getTijd(), kandidaatAfspraakDto.getDatum());
				MammaAfspraak dummyAfspraak = baseFactory.maakDummyAfspraak(uitnodiging, vanaf, capaciteitBlok, standplaatsPeriode, verzettenReden);

				MammaAfspraakPanel afspraakPanel = new MammaAfspraakPanel("nieuweAfspraakPanel", dummyAfspraak, true)
				{
					@Override
					protected void wijzigMoment(AjaxRequestTarget target)
					{
						emptyNieuweAfspraakPanel(target);
					}
				};
				nieuweAfspraakPanel.replaceWith(afspraakPanel);
				nieuweAfspraakPanel = afspraakPanel;

				this.setVisible(false);
				target.add(nieuweAfspraakPanel);
				target.add(this);
			}

		};
		afspraakZoekenPanel.setOutputMarkupPlaceholderTag(true);

		add(afspraakZoekenPanel);
	}

	@Override
	public void validate()
	{
		if (nieuweAfspraakPanel instanceof EmptyPanel)
		{
			error("U heeft geen nieuwe afspraak gekozen.");
		}
		else
		{
			((AbstractClientContactActiePanel) nieuweAfspraakPanel).validate();
			checkBestaatCapaciteitBlokNog(null, getNieuweAfspraak().getCapaciteitBlok().getId());
		}
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		MammaAfspraak afspraak = getNieuweAfspraak();
		SimpleDateFormat dateFormat = new SimpleDateFormat("EEEE dd-MM-yyyy HH:mm");
		return List.of(String.format("De afspraak wordt verzet naar %s in %s met %s", dateFormat.format(afspraak.getVanaf()),
			afspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats().getNaam(),
			afspraak.getCapaciteitBlok().getScreeningsEenheid().getNaam()));
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		if (hibernateService.get(MammaCapaciteitBlok.class, getNieuweAfspraak().getCapaciteitBlok().getId()) == null)
		{
			throw new MammaTijdNietBeschikbaarException();
		}
		Map<ExtraOpslaanKey, Object> opslaanObjecten = super.getOpslaanObjecten();
		opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK, ModelProxyHelper.deproxy(getNieuweAfspraak()));
		opslaanObjecten.put(ExtraOpslaanKey.MAMMA_AFSPRAAK_FILTER, ((MammaAfspraakZoekenPanel) afspraakZoekenPanel).getFilterModel().getObject());
		if (nieuweAfspraakPanel instanceof AbstractClientContactActiePanel)
		{
			opslaanObjecten.putAll(((AbstractClientContactActiePanel) nieuweAfspraakPanel).getOpslaanObjecten());
		}
		return opslaanObjecten;
	}

	public MammaAfspraak getNieuweAfspraak()
	{
		return ((MammaAfspraakPanel) nieuweAfspraakPanel).getModelObject();
	}

	private boolean checkBestaatCapaciteitBlokNog(AjaxRequestTarget target, Long capaciteitBlokId)
	{
		if (hibernateService.get(MammaCapaciteitBlok.class, capaciteitBlokId) == null)
		{
			error(getString("tijd.niet.beschikbaar"));
			emptyNieuweAfspraakPanel(target);
			return false;
		}
		return true;
	}

	private void emptyNieuweAfspraakPanel(AjaxRequestTarget target)
	{
		Panel emptyPanel = new EmptyPanel("nieuweAfspraakPanel");
		emptyPanel.setOutputMarkupId(true);
		nieuweAfspraakPanel.replaceWith(emptyPanel);
		nieuweAfspraakPanel = emptyPanel;
		afspraakZoekenPanel.setVisible(true);
		if (target != null)
		{
			target.add(nieuweAfspraakPanel);
			target.add(afspraakZoekenPanel);
		}
	}
}
